module FsGll.Tests

open NUnit.Framework
open FsGll
open FsGll.PureParsers
open FsGll.InputStream
open FsGll.TestingCommon

module private ExtCalcCases = 
    let (=?) (a: float) b = System.Math.Abs(a - b) < 0.0001
    let case1 fn = 
        match fn ("a = 0; var = a + 1111.2       ;  a - var * 2 + 837 / (x-3)   ") with 
        | [Success(
            EPgm
                ([EAssign (EVar "a",EVal v0);
                EAssign (EVar "var",EP (EVar "a",'+',EVal v11112))],
                EP  (EVar "a",'-',
                    EP
                      (EM (EVar "var",'*',EVal v2),'+',
                        EM (EVal v837,'/',EP (EVar "x",'-',EVal v3))))))] 
            when v0 =? 0.0 && v11112 =? 1111.2 && v2 =? 2.0 && v3 =? 3.0 && v837 =? 837.0 -> true 
        | _ -> false

let private nnnExpectFails fn n = fn n |> List.forall (function Failure(_, _) -> true | _ -> false) |> Assert.IsTrue

let private nnnTest fn n = 
    match fn n with | [Success(s)] when s = String.replicate n "0" -> true | _ -> false
    |> Assert.IsTrue


#nowarn "0058"
[<Test>] let ``grammar NNN, non-pure, empty string`` () = nnnExpectFails TestsPure.runNnn 0
[<Test>] let ``grammar NNN, non-pure, 1ch`` () = nnnTest TestsPure.runNnn 1
[<Test>] let ``grammar NNN, non-pure, 10ch`` () = nnnTest TestsPure.runNnn 10
[<Test>] let ``grammar NNN, non-pure, 30ch`` () = nnnTest TestsPure.runNnn 30

[<Test>] let ``grammar NNN, pure, empty string`` () = nnnExpectFails TestsNonPure.runNnn 0
[<Test>] let ``grammar NNN, pure, 1ch`` () = nnnTest TestsNonPure.runNnn 1
[<Test>] let ``grammar NNN, pure, 10ch`` () = nnnTest TestsNonPure.runNnn 10
[<Test>] let ``grammar NNN, pure, 30ch`` () = nnnTest TestsNonPure.runNnn 30

[<Test>] let ``grammar ExtCalc, non-pure, case 1`` () = ExtCalcCases.case1 TestsNonPure.runExtCalc |> Assert.IsTrue
[<Test>] let ``grammar ExtCalc, non-pure (fslex), case 1`` () = ExtCalcCases.case1 TestsNonPure.runExtCalcFslex |> Assert.IsTrue

[<Test>] let ``grammar ExtCalc, pure, case 1`` () = ExtCalcCases.case1 TestsPure.runExtCalc |> Assert.IsTrue
[<Test>] let ``grammar ExtCalc, pure (fslex), case 1`` () = ExtCalcCases.case1 TestsPure.runExtCalcFslex |> Assert.IsTrue

open FsGll.Mutable.PriorityQueue
[<Test>] 
let ``priority queue test 1`` () = 
    let q = new PriorityQueue<int>((-))
    let initial = [1; 3; 5; 1; 10; 2; 13; 1; 1; 23; 9; 8; 7; 43] 
    List.iter q.Add initial
    let items = [0 .. initial.Length - 1] |> List.map (fun _ -> q.Pop ())
    let ck1 = (items = List.sort initial) 
    let ck2 = q.Count = 0
    let additional1 = [5; 3; 8; 1; 3]
    List.iter q.Add additional1
    let ck3 = q.Pop() = 1
    q.Add(-1)
    let ck4 = q.Pop() = -1
    let ck5 = q.Pop() = 3
    ([ck1;ck2;ck3;ck4;ck5] |> List.forall ((=)true)) |> Assert.IsTrue