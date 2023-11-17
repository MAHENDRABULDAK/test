// Define the list of salaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Function to calculate tax
let calculateTax salary =
    match salary with
    | _ when salary <= 49020 -> salary * 15 / 100
    | _ when salary <= 98040 -> (49020 * 15 / 100) + ((salary - 49020) * 205 / 1000)
    | _ when salary <= 151978 -> (49020 * 15 / 100) + ((98040 - 49020) * 205 / 1000) + ((salary - 98040) * 26 / 100)
    | _ when salary <= 216511 -> (49020 * 15 / 100) + ((98040 - 49020) * 205 / 1000) + ((151978 - 98040) * 26 / 100) + ((salary - 151978) * 29 / 100)
    | _ -> (49020 * 15 / 100) + ((98040 - 49020) * 205 / 1000) + ((151978 - 98040) * 26 / 100) + ((216511 - 151978) * 29 / 100) + ((salary - 216511) * 33 / 100)

// Map operation: Calculate the tax for all salaries
let taxedSalaries = salaries |> List.map calculateTax

// Filter salaries between $50,000 and $100,000 and sum them
let sumMidRangeSalaries = 
    salaries 
    |> List.filter (fun salary -> salary > 50000 && salary < 100000) 
    |> List.fold (+) 0

// Filter and map: Filter salaries less than $49,020 and add $20,000
let adjustedSalaries = 
    salaries 
    |> List.filter (fun salary -> salary < 49020) 
    |> List.map (fun salary -> salary + 20000)

// Tail-recursive function to sum multiples of 3
let sumOfMultiplesOfThree max =
    let rec sumHelper accumulator currentMax =
        if currentMax <= 0 then
            accumulator
        else
            sumHelper (accumulator + currentMax) (currentMax - 3)
    sumHelper 0 max

// Testing the functions
printfn "Salaries after Tax: %A" taxedSalaries
printfn "Sum of Mid-Range Salaries: %d" sumMidRangeSalaries
printfn "Adjusted Salaries: %A" adjustedSalaries
let sumMultiples = sumOfMultiplesOfThree 27
printfn "Sum of multiples of 3 up to 27 is: %d" sumMultiples
