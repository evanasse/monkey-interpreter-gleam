pub type OpPrecedence =
  Int

pub const call: OpPrecedence = 6

pub const prefix: OpPrecedence = 5

pub const product: OpPrecedence = 4

pub const sum: OpPrecedence = 3

pub const lesser_greater: OpPrecedence = 2

pub const equals: OpPrecedence = 1

pub const lowest: OpPrecedence = 0
