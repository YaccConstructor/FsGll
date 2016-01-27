module FsGll.Tests

open FsGll
open NUnit.Framework

[<Test>]
let ``hello returns 42`` () =
  let result = 42
  Assert.AreEqual(42,result)
