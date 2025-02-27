import gleeunit/should
import monkey/object

pub fn string_hash_key_test() {
  let hello1 = object.String(value: "Hello World")
  let hello2 = object.String(value: "Hello World")
  let diff1 = object.String(value: "My name is Johnny")
  let diff2 = object.String(value: "My name is Johnny")

  object.hash_key(hello1)
  |> should.equal(object.hash_key(hello2))

  object.hash_key(diff1)
  |> should.equal(object.hash_key(diff2))

  object.hash_key(hello1)
  |> should.not_equal(object.hash_key(diff1))
}

pub fn interger_hash_key_test() {
  let value1 = object.Integer(value: 1)
  let value2 = object.Integer(value: 1)
  let diff1 = object.Integer(value: 2)
  let diff2 = object.Integer(value: 2)

  object.hash_key(value1)
  |> should.equal(object.hash_key(value2))

  object.hash_key(diff1)
  |> should.equal(object.hash_key(diff2))

  object.hash_key(value1)
  |> should.not_equal(object.hash_key(diff1))
}

pub fn boolean_hash_key_test() {
  let bool1 = object.Boolean(value: True)
  let bool2 = object.Boolean(value: True)
  let diff1 = object.Boolean(value: False)
  let diff2 = object.Boolean(value: False)

  object.hash_key(bool1)
  |> should.equal(object.hash_key(bool2))

  object.hash_key(diff1)
  |> should.equal(object.hash_key(diff2))

  object.hash_key(bool1)
  |> should.not_equal(object.hash_key(diff1))
}
