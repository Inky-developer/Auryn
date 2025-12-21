use std::io::Write;

use crate::{
    java::{
        assembler::{ConstantValue, Instruction, Primitive},
        class::{
            self, Comparison, JumpPoint, StackMapTableAttribute, TypeCategory, VerificationTypeInfo,
        },
        constant_pool_builder::ConstantPoolBuilder,
        symbolic_evaluation::{Frame, SymbolicEvaluator},
    },
    utils::{fast_map::FastMap, graph::Graph},
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BasicBlockId(pub usize);

#[derive(Debug, Default)]
pub enum BlockFinalizer {
    /// Compares the first two stack entries according to `comparison`.
    BranchIntegerCmp {
        comparison: Comparison,
        positive_block: BasicBlockId,
        negative_block: BasicBlockId,
    },
    /// Compares the first stack entry with 0 according to `comparison`.
    BranchInteger {
        comparison: Comparison,
        positive_block: BasicBlockId,
        negative_block: BasicBlockId,
    },
    Goto(BasicBlockId),
    #[default]
    ReturnNull,
}

impl BlockFinalizer {
    pub fn targets(&self) -> impl Iterator<Item = BasicBlockId> {
        let targets = match self {
            BlockFinalizer::Goto(basic_block_id) => [Some(*basic_block_id), None],
            BlockFinalizer::BranchIntegerCmp {
                comparison: _,
                positive_block,
                negative_block,
            } => [Some(*positive_block), Some(*negative_block)],
            BlockFinalizer::BranchInteger {
                comparison: _,
                positive_block,
                negative_block,
            } => [Some(*positive_block), Some(*negative_block)],
            BlockFinalizer::ReturnNull => [None, None],
        };

        targets.into_iter().filter_map(|it| it)
    }
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub finalizer: BlockFinalizer,
}

#[derive(Debug)]
pub struct SourceGraph {
    graph: Vec<BasicBlock>,
    pub current: BasicBlockId,
}

impl Default for SourceGraph {
    fn default() -> Self {
        Self {
            graph: vec![BasicBlock::default()],
            current: BasicBlockId(0),
        }
    }
}

impl SourceGraph {
    pub fn get(&self, id: BasicBlockId) -> &BasicBlock {
        &self.graph[id.0]
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.graph[self.current.0]
    }

    pub fn add_block(&mut self) -> BasicBlockId {
        let id = self.graph.len();
        self.graph.push(BasicBlock::default());
        BasicBlockId(id)
    }

    pub fn add(&mut self, instruction: Instruction) {
        self.current_block_mut().instructions.push(instruction);
    }

    /// Converts the graph of basic blocks into the java bytecode representation
    /// by assembling the blocks into a viable order and computing stack map frames
    pub fn assemble(
        self,
        constant_pool: &'_ mut ConstantPoolBuilder,
        arguments: Vec<VerificationTypeInfo>,
    ) -> (Vec<class::Instruction>, StackMapTableAttribute) {
        let mut context = AssemblyContext(constant_pool);
        let mut graph = build_graph(self.graph);
        let block_order = graph.order_topologically(BasicBlockId(0));
        // The block order gets computed by following the forward edges,
        // but now we want to query the blocks from which a block can be reached, so the graph must be inverted.
        graph.invert_edges();

        let mut frames = context.compute_frames(&graph, &block_order, arguments);

        let mut block_to_byte_offset: FastMap<BasicBlockId, u16> = FastMap::default();
        let mut current_offset: u16 = 0;
        for (index, block_id) in block_order.iter().copied().enumerate() {
            block_to_byte_offset.insert(block_id, current_offset);

            let block = graph.get_vertex(block_id).expect("Should exist");
            current_offset += context.measure_len(&block.instructions);

            if let Some(finalizer_instruction) = context.convert_finalizer_instruction(
                &block.finalizer,
                block_order.get(index + 1).copied(),
                |_| JumpPoint(0),
            ) {
                current_offset += measure_class_len(&finalizer_instruction);
            }
        }

        let mut code = Vec::new();
        for (index, block_id) in block_order.iter().copied().enumerate() {
            let next_block_id = block_order.get(index + 1).copied();
            let block = graph.get_vertex(block_id).expect("Should exist");
            for instruction in &block.instructions {
                context.convert_instruction(&instruction, |i| code.push(i));
            }
            if let Some(finalizer) =
                context.convert_finalizer_instruction(&block.finalizer, next_block_id, |block_id| {
                    JumpPoint(
                        *block_to_byte_offset
                            .get(&block_id)
                            .expect("Offset should be computed"),
                    )
                })
            {
                code.push(finalizer);
            }
        }

        let verification_frames = block_order
            .iter()
            .copied()
            .map(|id| {
                (
                    *block_to_byte_offset
                        .get(&id)
                        .expect("Offset should be computed"),
                    frames.remove(&id).expect("Frame should exist"),
                )
            })
            .collect();

        (code, convert_verification_frames(verification_frames))
    }
}

struct AssemblyContext<'a>(&'a mut ConstantPoolBuilder);

impl AssemblyContext<'_> {
    fn compute_frames(
        &mut self,
        graph: &Graph<BasicBlockId, BasicBlock>,
        block_order: &[BasicBlockId],
        function_arguments: Vec<VerificationTypeInfo>,
    ) -> FastMap<BasicBlockId, Frame> {
        struct BlockFrames {
            at_start: Frame,
            at_end: Frame,
        }

        // dbg!(&block_order, graph);

        // Theoretically we probably need a fixpoint iteration for loops, but lets just see for how long
        // just assuming that loops don't change works
        let mut visited_blocks: FastMap<BasicBlockId, BlockFrames> = FastMap::default();
        for id in block_order.iter().copied() {
            let calling_blocks = graph.edges(id);
            let mut caller_frames =
                calling_blocks.filter_map(|caller_id| visited_blocks.get(&caller_id));

            let frame_at_start_of_block = if id.0 == 0 {
                Frame {
                    locals: function_arguments.clone(),
                    stack: Vec::new(),
                }
            } else {
                let first_frame = caller_frames
                    .next()
                    .expect("This frame should not be unreachable (?)")
                    .at_end
                    .clone();
                caller_frames.fold(first_frame, |acc, block_frames| {
                    acc.combine(&block_frames.at_end)
                })
            };

            let mut evaluator = SymbolicEvaluator::from(frame_at_start_of_block.clone());
            let block = graph.get_vertex(id).unwrap();
            evaluator.eval_block(block, &mut self.0);
            let frame_at_end_of_block = Frame {
                locals: evaluator.locals,
                stack: evaluator.stack,
            };

            visited_blocks.insert(
                id,
                BlockFrames {
                    at_start: frame_at_start_of_block,
                    at_end: frame_at_end_of_block,
                },
            );
        }

        visited_blocks
            .into_iter()
            .map(|(k, v)| (k, v.at_start))
            .collect()
    }

    /// Returns the length of the instructions compiled into bytecode
    fn measure_len(&mut self, instructions: &[Instruction]) -> u16 {
        let mut writer = VoidWriter::default();
        for instruction in instructions {
            self.convert_instruction(&instruction, |i| i.serialize(&mut writer, 0).unwrap());
        }
        writer.len.try_into().unwrap()
    }

    fn convert_instruction(
        &mut self,
        instruction: &Instruction,
        mut on_instruction: impl FnMut(class::Instruction),
    ) {
        match instruction {
            Instruction::GetStatic {
                class_name,
                name,
                field_type,
            } => {
                let field_ref_index =
                    self.0
                        .add_field_ref(class_name.clone(), name.clone(), field_type.to_string());
                on_instruction(class::Instruction::GetStatic(field_ref_index))
            }
            Instruction::InvokeVirtual {
                class_name,
                name,
                method_type,
            } => {
                let method_ref_index = self.0.add_method_ref(
                    class_name.clone(),
                    name.clone(),
                    method_type.to_string(),
                );
                on_instruction(class::Instruction::InvokeVirtual(method_ref_index))
            }
            Instruction::LoadConstant { value } => {
                let constant_index = match value {
                    ConstantValue::String(string) => self.0.add_string(string.clone()),
                    ConstantValue::Integer(integer) => self.0.add_integer(*integer),
                };
                let constant_index = constant_index
                    .0
                    .get()
                    .try_into()
                    .expect("TODO: Implement support for higher indexes");
                on_instruction(class::Instruction::Ldc(constant_index))
            }
            Instruction::ReturnNull => on_instruction(class::Instruction::Return),
            Instruction::IAdd => on_instruction(class::Instruction::IAdd),
            Instruction::IMul => on_instruction(class::Instruction::IMul),
            Instruction::Store(id) => on_instruction(match id.r#type {
                Primitive::Integer => class::Instruction::IStore(id.index),
            }),
            Instruction::Load(id) => on_instruction(match id.r#type {
                Primitive::Integer => class::Instruction::ILoad(id.index),
            }),
            Instruction::Nop => on_instruction(class::Instruction::Nop),
            Instruction::Pop(category) => match category {
                TypeCategory::Normal => on_instruction(class::Instruction::Pop),
                TypeCategory::Big => on_instruction(class::Instruction::Pop2),
            },
        }
    }

    fn convert_finalizer_instruction(
        &self,
        finalizer: &BlockFinalizer,
        next_implicit_block_id: Option<BasicBlockId>,
        mut resolve_jump_point: impl FnMut(BasicBlockId) -> JumpPoint,
    ) -> Option<class::Instruction> {
        match finalizer {
            BlockFinalizer::Goto(target) => {
                if Some(*target) != next_implicit_block_id {
                    Some(class::Instruction::Goto(resolve_jump_point(*target)))
                } else {
                    None
                }
            }
            BlockFinalizer::ReturnNull => Some(class::Instruction::Return),
            BlockFinalizer::BranchIntegerCmp {
                comparison,
                positive_block,
                negative_block,
            } => Some(match next_implicit_block_id {
                Some(id) if id == *positive_block => class::Instruction::IfICmp {
                    comparison: comparison.invert(),
                    jump_point: resolve_jump_point(*negative_block),
                },
                Some(id) if id == *negative_block => class::Instruction::IfICmp {
                    comparison: *comparison,
                    jump_point: resolve_jump_point(*positive_block),
                },
                _ => panic!("Either positive block negative block has to be implicit!"),
            }),
            BlockFinalizer::BranchInteger {
                comparison,
                positive_block,
                negative_block,
            } => Some(match next_implicit_block_id {
                Some(id) if id == *positive_block => class::Instruction::IfI {
                    comparison: comparison.invert(),
                    jump_point: resolve_jump_point(*negative_block),
                },
                Some(id) if id == *negative_block => class::Instruction::IfI {
                    comparison: *comparison,
                    jump_point: resolve_jump_point(*positive_block),
                },
                _ => panic!("Either positive block negative block has to be implicit!"),
            }),
        }
    }
}

/// Builds a graph from the basic blocks.
/// An edge from block B to block A will be created if block A can directly go to block B.
/// This allows for a quick lookup of which blocks target a given block.
fn build_graph(blocks: Vec<BasicBlock>) -> Graph<BasicBlockId, BasicBlock> {
    let mut graph = Graph::default();

    for (index, block) in blocks.into_iter().enumerate() {
        let id = BasicBlockId(index);
        for next_id in block.finalizer.targets() {
            graph.connect(id, next_id);
        }
        graph.insert(id, block);
    }

    graph
}

/// Convertes a list of frames and their byte offsets into the final [`StackMapTableAttribute`]
fn convert_verification_frames(verification_frames: Vec<(u16, Frame)>) -> StackMapTableAttribute {
    let mut current_offset = 0u16;
    StackMapTableAttribute {
        entries: verification_frames
            .into_iter()
            .enumerate()
            .filter_map(move |(index, (offset, Frame { locals, stack }))| {
                if offset == current_offset && index != 0 {
                    return None;
                }
                // An offset of 0 indicates that the frame applies to the next instruction, so we need to subtract 1 here, unless this is the first frame
                // In which case this must not be done according to spec.
                let offset_to_last: u16 = if index == 0 {
                    offset
                } else {
                    (offset - current_offset - 1)
                        .try_into()
                        .expect("Should not to to big")
                };
                current_offset = offset;
                Some(class::StackMapFrame::Full {
                    offset_delta: offset_to_last,
                    locals,
                    stack,
                })
            })
            .collect(),
    }
}

/// A writer that voids everything but keeps track of the total number of bytes
#[derive(Debug, Default)]
struct VoidWriter {
    pub len: usize,
}

impl Write for VoidWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.len += buf.len();
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn measure_class_len(class_instruction: &class::Instruction) -> u16 {
    let mut writer = VoidWriter::default();
    // The current index should not be relevant when just measuring instructions
    class_instruction.serialize(&mut writer, 0).unwrap();
    writer.len.try_into().unwrap()
}
