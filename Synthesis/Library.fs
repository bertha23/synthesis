module Synthesis

open System.Linq.Expressions
open System.Net.Sockets

let abelar v = (v%12=0) && (v>12) && (v<3097)


let area b h = 
    match b<0.0|| h<0.0 with 
    |true-> failwith "Out of range exception"
    |_-> (b/2.0)*h
    

let zollo x =
    match x>0 with
    |true-> x*2
    |_-> x* -1
    

let min n m =
    match m < n with 
    |true-> m
    |false -> n
    

let max a b =
    match a>b || a=b with
    |true-> a
    |_->b
  

let ofTime h m s = (h*3600)+(m*60)+(s)
    

let toTime s =
    match s>0 with
    |true-> (s/3600),((s%3600)/60),(s%60)
    |_->(0,0,0)
   

let rec digits num =
    match num<10 with
    |true-> 1
    |_->1+ digits(num/10)
   
   

let minmax _ =
    failwith "Not implemented"

let isLeap yr =
    match yr<1582 with
    |true-> failwith "Not implemented"
    |false-> match (yr%100=0) with
             |true-> failwith "invalid"
             |false->
                (yr%4=0) || (yr%400=0)

let month y =
    match y<1 || y>12 with
    |false-> match y with 
             |1-> ("January",31)
             |2-> ("February",28)
             |3-> ("March",31)
             |4-> ("April",30)
             |5->("May",31)
             |6->("June",30)
             |7->("July",31)
             |8->("August",31)
             |9->("September",30)
             |10->("October",31)
             |11->("November",30)
             |12->("December",31)
    |_-> failwith "Not implemented"

let toBinary bin = 
    match bin<0 with 
    |true->failwith "Not implemented"
    |false-> 
        let rec binaryconvertor c acc=
            match c <> 0 with
            |true-> match (c%2) with
                    |0->binaryconvertor(c/2)("0"+acc)
                    |1->binaryconvertor(c/2)("1"+acc)
            |false->acc
        match bin =0 with
        |true -> "0"
        |false-> binaryconvertor bin ""

let bizFuzz _ =
    failwith "Not implemented"

let monthDay d y =
    match d > 0 && d <= 366 && y >= 1582 with
    |false-> failwith "invalid entry"
    |true->
        let rec checkmonth s h acc =
            let mon,day = month h
            match isLeap y = true with
            |true->
                match mon = "Frebruary" with
                |true->
                    match s > acc && s <= (acc+day+1) with
                    |true-> mon
                    |false-> checkmonth s (h+1) (acc+day+1)
                |false->
                    match s > acc && s <= (acc+day) with
                    |true-> mon
                    |false-> checkmonth s (h+1) (acc+day)
            |false->
                match isLeap y = false && d < 366 with
                |false-> failwith " invalid entry"
                |true->
                    match s > acc && s <= (acc+day) with
                    |true-> mon
                    |false-> checkmonth s (h+1) (acc+day)
        let mon,day = month 1
        match d > 0 && d <= day with
        |true->"January"
        |false-> checkmonth d 2 31

let coord _ =
    failwith "Not implemented"