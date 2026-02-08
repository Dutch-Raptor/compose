use std::num::NonZeroU64;

/// A unique identifier for a symbol, represented as a `NonZeroU64`.
///
/// The `SymbolId` structure is used to uniquely identify symbols within
/// a system. It is a lightweight wrapper around a `NonZeroU64`, ensuring
/// that the identifier is always non-zero.
///
/// # Derive Traits
/// - `Debug`: Allows for formatting the `SymbolId` for debugging purposes.
/// - `Clone`: Enables the ability to create a copy of the `SymbolId`.
/// - `Copy`: Allows `SymbolId` to be copied rather than moved.
/// - `PartialEq` and `Eq`: Provides equality comparison for `SymbolId`.
/// - `Hash`: Allows the `SymbolId` to be used as a key in hash-based collections,
///   such as `HashMap` or `HashSet`.
///
/// # Examples
/// ```rust
/// use std::num::NonZeroU64;
///
/// let non_zero_value = NonZeroU64::new(1).unwrap();
/// let symbol_id = SymbolId(non_zero_value);
///
/// println!("{:?}", symbol_id); // Outputs: SymbolId(1)
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(NonZeroU64);