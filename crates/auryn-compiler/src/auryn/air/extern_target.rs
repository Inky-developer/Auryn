#[derive(Debug, Clone, Copy)]
pub enum ExternTarget {
    Intrinsic,
    Java,
    Unknown,
}

impl ExternTarget {
    pub fn is_intrinsic(&self) -> bool {
        matches!(self, ExternTarget::Intrinsic)
    }

    pub fn supports_extern_types(&self) -> bool {
        use ExternTarget::*;
        match self {
            Intrinsic => false,
            Java | Unknown => true,
        }
    }

    pub fn supports_extern_functions(&self) -> bool {
        use ExternTarget::*;
        match self {
            Java => false,
            Intrinsic | Unknown => true,
        }
    }

    pub fn supports_type_parameters(&self) -> bool {
        use ExternTarget::*;
        match self {
            Java => false,
            Intrinsic | Unknown => true,
        }
    }
}

impl ExternTarget {
    pub fn from_str(value: &str) -> Self {
        match value {
            "intrinsics" => Self::Intrinsic,
            "java" => Self::Java,
            _ => Self::Unknown,
        }
    }
}
