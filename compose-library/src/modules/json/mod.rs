use std::collections::HashMap;

/// because normal values contain heap references they cannot properly implement SerDe's serialize and
/// deserialize. This enum is a `dereferenced` version
pub enum SerializableValue {
    Int(i64),
    Bool(bool),
    Unit,
    Str(String),
    Array(Vec<SerializableValue>),
    Map(HashMap<String, SerializableValue>),
}





