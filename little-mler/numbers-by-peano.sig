signature NUMBERS_BY_PEANO =
sig
    type number
    exception Too_small
    val succ: number -> number
    val pred: number -> number
    val is_zero: number -> bool
end
