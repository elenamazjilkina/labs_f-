open System

//==============================================ПРАКТИКУМ 1====================================================

//Дано четырехзначное натуральное число n. Найти сумму первой и последней цифры этого числа.
module lab1_num7 = 

    let sum values = values / 1000 + values % 10

    printfn "%A" (sum 6891)


//Написать функцию нахождения площади, периметра и длину диагонали прямоугольника.
module lab1_num15 = 

    let rectangle (a: float, b: float) = 
        let perimetr = 2.0 * (a + b)
        let area = a * b
        let diagonal = sqrt(a**2.0 + b**2.0)
        perimetr, area, diagonal
    
    printfn "%A" (rectangle (5.0, 2.0))


//Определить при помощи конструкции match. В какой плоскости расположена точка. A(x, y)
module lab1_num23 = 

    let coordinateQuarter x y =
        match x, y with 
        | x, y when x > 0 && y > 0 -> "I"
        | x, y when x > 0 && y < 0 -> "IV"
        | x, y when x < 0 && y < 0 -> "III"
        | x, y when x < 0 && y > 0 -> "II"
        | _, _ -> "???"

    printfn "%A" (coordinateQuarter 5 -2) 


//Дано число N. Составить список из чисел, не превышающих N и не являющихся его делителями.
module lab2_num7 =  

    let listN x =
        [ for i in 2 .. x do 
            if x % i <> 0 then 
                yield i ] 
        
    printfn "%A" (listN 20) 


//Создать список из простых чисел.
module lab2_num15 =  

    let rec primes = function
        [] -> []
        | h::t when h = 2 -> primes(List.filter (fun x->x%h>0) t)
        | h::t -> h::primes(List.filter (fun x->x%h>0) t)
        
    printfn "%A" (primes [2..15]) 


//Сформировать список по принципу [1; 2; 6; 24; 120; 720].
module lab2_num23 =  

    let rule = 
        let factorial n = 
            [1..n] |> List.fold (*) 1
        [1..6] |> List.map factorial 


//Найти разность максимального и минимального элементов списка
module lab3_num7 = 

    let max_min xs = List.max xs - List.min xs  

    printfn "%A" (max_min [-1; 0; 3])


//Определить количество ненулевых элементов списка
module lab3_num15 = 

    let is_null xs = 
        List.filter (fun x -> x <> 0) xs
        |> List.length

    printfn "%A" (is_null [-1; 0; 3; 5; 2; 0; 99])


//Дан список. Создать второй, состоящий из true и false, при условии, что на соответствующих местах четных чисел из первого будет стоять – true, на местах нечетных – false
module lab3_num23 = 

    let true_false_list xs = 
        List.map (fun x -> if x%2<>0 then false else true) xs

    printfn "%A" (true_false_list [-1; 8; 3; 5; 2; 0; 99])


//Для последовательности целых чисел найти сумму и количество элементов, кратных 5 и не кратных 7
module lab4_num7 = 

    let usl pos = pos % 5 = 0 && pos % 7 <> 0
    let qwe = Seq.toList(Seq.filter usl (seq { for i = 0 to 100 do yield i }))
    printfn "%A" (Seq.sum qwe, Seq.length qwe)


//С помощью последовательности вывести все пути к не текстовым файлам в указанном каталоге и подкаталогах
module lab4_num15 = 
    let rec allFilesUnder basePath =
        seq {
        yield! System.IO.Directory.GetFiles(basePath)
        for subdir in System.IO.Directory.GetDirectories(basePath) do
            yield! allFilesUnder subdir
        }
    let sp = allFilesUnder @"C:\Users\Admin\Desktop\Учеба\Физкультура"
    for c in sp do
        if not(c.Contains("doc") || c.Contains("txt") || c.Contains("rtf")) then
            printfn "%A" c


//Выяснить есть ли в каталоге и подкаталогах файл с введенным названием
module lab4_num23 =
    let rec allFilesUnder basePath =
        seq {
        yield! System.IO.Directory.GetFiles(basePath)
        for subdir in System.IO.Directory.GetDirectories(basePath) do
            yield! allFilesUnder subdir
        }
    let sp = allFilesUnder @"C:\Users\Admin\Desktop\Учеба\Физкультура"
    let spis = seq {for c in sp do if (c.Contains("реферат")) then yield c}
    if Seq.length spis <> 0 then
        Seq.iter (printfn "%A") spis
        else printfn "%A" ("Файла с таким названием не существует")


//...тут должно быть дерево...



//=======================================ПРАКТИКУМ 2==================================================

(* Одну из исполняемых операций перегрузите (например, если в задании требуется возведение в квадрат, создайте отдельно такую операцию и примените ее). Вывод данных организуйте до 2-х знаков после запятой, со знаком числа:
1. for...in  (используйте последовательности (или списки))
2. for...to
3. while *)

//Все числа от 100 до 50 через каждые n (введено пользователем) , поделить на 2
//module part1_num7 =
//    let (/) x = x / 2.
//    let mutable x = ""
//    printfn "Enter a number:"
//    let r =
//        try
//            x <- System.Console.ReadLine();
//            Some (float(x))
//        with
//            | :? System.FormatException -> printfn "Invalid number!"
//                                           None
//    if r.IsSome then
//        for i in seq {100. .. -float(x) .. 50.} do
//            Console.WriteLine ("{0:0.##}", (/) i)

////как сделать шаг???
//        for i = 50 to 100 do 
//            Console.WriteLine ("{0:0.##}", (/) (float(i)))

//        let mutable i = 100.
//        while i > 50. do
//             Console.WriteLine ("{0:0.##}", (/) i)
//             i <- i - float(x)


//Выведите все числа в промежутке между (х + у)*2 и (х * у)*3, если х=3, у=2.
module part2_num7 = 
    let allnum x y =
        let z = seq {(x + y)*2..(x * y)*3}
        Seq.iter (printfn "%A") z
    allnum 3 2

//Реализация математической функции. Ввод данных осуществляет пользователь. Функция вычисляется лениво.
//module part3_num7 = 
//    let f (x, y) = exp(sqrt(log((x**2. + y**2.)/((cos x)**2. + cos(x**2.)))))
//    let mutable x = ""
//    let mutable y = ""
//    printfn "Enter a number of X:"
//    let r =
//        try
//            x <- System.Console.ReadLine();
//            Some (float(x))
//        with
//            | :? System.FormatException -> printfn "Invalid number!"
//                                           None
//    if r.IsSome then
//        printfn "Enter a number of Y:"
//        let t =
//            try
//                y <- System.Console.ReadLine();
//                Some (float(y))
//            with
//                | :? System.FormatException -> printfn "Invalid number!"
//                                               None
//        if t.IsSome then
//            let result = lazy (f (float(x), float(y)))
//            Console.WriteLine ("{0:0.##}", result.Force())


(*Дано трехзначное целое число 𝑋. Все цифры должны быть отличны от нуля. Вывести новое трехзначное число 𝑌,
каждая цифра которого является остатком от деления суммы всех цифр числа 𝑋 на соответствующую цифру числа 𝑋. 
Например, 𝑋 = 256, тогда 𝑌 = 131 *)
module part4_num7 =
    Console.WriteLine ("Введите трехзначное число: ")
    let mutable x = ""
    x <- Console.ReadLine()
    let sum = List.sum [int(x) / 100; (int(x) / 10) % 10; int(x) % 10]
    Console.WriteLine ((sum % (int(x) / 100)) * 100 + (sum % (int(x) / 10) % 10) * 10 + sum % (int(x) % 10))


Console.ReadKey() |> ignore

0