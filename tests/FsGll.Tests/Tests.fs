module FsGll.Tests

open FsGll
open NUnit.Framework
open FsGll.PureParsers
open FsGll.InputStream
open FsGll.TestingCommon

module private ExtCalcCases = 
    let (=?) (a: float) b = System.Math.Abs(a - b) < 0.001
    let case1 fn = 
        match fn ("a = 0; var = a + 1; a - var * 2 + 837 / (x-3)   ") with 
        | [Success(
            EPgm
                ([EAssign (EVar "a",EVal v0);
                EAssign (EVar "var",EP (EVar "a",'+',EVal v1))],
                EP  (EVar "a",'-',
                    EP
                      (EM (EVar "var",'*',EVal v2),'+',
                        EM (EVal v837,'/',EP (EVar "x",'-',EVal v3))))))] 
            when v0 =? 0.0 && v1 =? 1.0 && v2 =? 2.0 && v3 =? 3.0 && v837 =? 837.0 -> true 
        | _ -> false

let private nnnExpectFails fn n = fn n |> List.forall (function Failure(_, _) -> true | _ -> false) |> Assert.IsTrue

let private nnnTest fn n = 
    match fn n with | [Success(s)] when s = String.replicate n "0" -> true | _ -> false
    |> Assert.IsTrue


#nowarn "0058"
[<Test>] let ``grammar NNN, non-pure, empty string`` () = nnnExpectFails TestsPure.runNnn 0
[<Test>] let ``grammar NNN, non-pure, 1ch`` () = nnnTest TestsPure.runNnn 1
[<Test>] let ``grammar NNN, non-pure, 10ch`` () = nnnTest TestsPure.runNnn 10
[<Test>] let ``grammar NNN, non-pure, 100ch`` () = nnnTest TestsPure.runNnn 30

[<Test>] let ``grammar NNN, pure, empty string`` () = nnnExpectFails TestsNonPure.runNnn 0
[<Test>] let ``grammar NNN, pure, 1ch`` () = nnnTest TestsNonPure.runNnn 1
[<Test>] let ``grammar NNN, pure, 10ch`` () = nnnTest TestsNonPure.runNnn 10
[<Test>] let ``grammar NNN, pure, 100ch`` () = nnnTest TestsNonPure.runNnn 30

[<Test>] let ``grammar ExtCalc, non-pure, case 1`` () = ExtCalcCases.case1 TestsNonPure.runExtCalc |> Assert.IsTrue
[<Test>] let ``grammar ExtCalc, non-pure (fslex), case 1`` () = ExtCalcCases.case1 TestsNonPure.runExtCalcFslex |> Assert.IsTrue

[<Test>] let ``grammar ExtCalc, pure, case 1`` () = ExtCalcCases.case1 TestsPure.runExtCalc |> Assert.IsTrue
[<Test>] let ``grammar ExtCalc, pure (fslex), case 1`` () = ExtCalcCases.case1 TestsPure.runExtCalcFslex |> Assert.IsTrue