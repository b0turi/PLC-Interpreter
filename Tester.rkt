(require "Interpreter.rkt")

(cond
  ((not (= (interpret "test/Part 1/1.txt") 150)) "Part 1 Test 1 Failed")
  ((not (= (interpret "test/Part 1/2.txt") -4)) "Part 1 Test 2 Failed")
  ((not (= (interpret "test/Part 1/3.txt") 10)) "Part 1 Test 3 Failed")
  ((not (= (interpret "test/Part 1/4.txt") 16)) "Part 1 Test 4 Failed")
  ((not (= (interpret "test/Part 1/5.txt") 220)) "Part 1 Test 5 Failed")
  ((not (= (interpret "test/Part 1/6.txt") 5)) "Part 1 Test 6 Failed")
  ((not (= (interpret "test/Part 1/7.txt") 6)) "Part 1 Test 7 Failed")
  ((not (= (interpret "test/Part 1/8.txt") 10)) "Part 1 Test 8 Failed")
  ((not (= (interpret "test/Part 1/9.txt") 5)) "Part 1 Test 9 Failed")
  ((not (= (interpret "test/Part 1/10.txt") -39)) "Part 1 Test 10 Failed")
  ((not (eq? (interpret "test/Part 1/15.txt") 'true)) "Part 1 Test 15 Failed")
  ((not (= (interpret "test/Part 1/16.txt") 100)) "Part 1 Test 16 Failed")
  ((not (eq? (interpret "test/Part 1/17.txt") 'false)) "Part 1 Test 17 Failed")
  ((not (eq? (interpret "test/Part 1/18.txt") 'true)) "Part 1 Test 18 Failed")
  ((not (= (interpret "test/Part 1/19.txt") 128)) "Part 1 Test 19 Failed")
  ((not (= (interpret "test/Part 1/20.txt") 12)) "Part 1 Test 20 Failed")
  ((not (= (interpret "test/Part 1/21.txt") 30)) "Part 1 Test 21 Failed")
  ((not (= (interpret "test/Part 1/22.txt") 11)) "Part 1 Test 22 Failed")
  ((not (= (interpret "test/Part 1/23.txt") 1106)) "Part 1 Test 23 Failed")
  ((not (= (interpret "test/Part 1/24.txt") 12)) "Part 1 Test 24 Failed")
  ((not (= (interpret "test/Part 1/25.txt") 16)) "Part 1 Test 25 Failed")
  ((not (= (interpret "test/Part 1/26.txt") 72)) "Part 1 Test 26 Failed")
  ((not (= (interpret "test/Part 1/27.txt") 21)) "Part 1 Test 27 Failed")
  ((not (= (interpret "test/Part 1/28.txt") 164)) "Part 1 Test 28 Failed")
  ((not (= (interpret "test/Part 2/1.txt") 20)) "Part 2 Test 1 Failed")
  ((not (= (interpret "test/Part 2/2.txt") 164)) "Part 2 Test 2 Failed")
  ((not (= (interpret "test/Part 2/3.txt") 32)) "Part 2 Test 3 Failed")
  ((not (= (interpret "test/Part 2/4.txt") 2)) "Part 2 Test 4 Failed")
  ((not (= (interpret "test/Part 2/6.txt") 25)) "Part 2 Test 6 Failed")
  ((not (= (interpret "test/Part 2/7.txt") 21)) "Part 2 Test 7 Failed")
  ((not (= (interpret "test/Part 2/8.txt") 6)) "Part 2 Test 8 Failed")
  ((not (= (interpret "test/Part 2/9.txt") -1)) "Part 2 Test 9 Failed")
  ((not (= (interpret "test/Part 2/10.txt") 789)) "Part 2 Test 10 Failed")
  ((not (= (interpret "test/Part 2/14.txt") 12)) "Part 2 Test 14 Failed")
  ((not (= (interpret "test/Part 2/15.txt") 125)) "Part 2 Test 15 Failed")
  ((not (= (interpret "test/Part 2/16.txt") 110)) "Part 2 Test 16 Failed")
  ((not (= (interpret "test/Part 2/17.txt") 2000400)) "Part 2 Test 17 Failed")
  ((not (= (interpret "test/Part 2/18.txt") 101)) "Part 2 Test 18 Failed")
  ((not (= (interpret "test/Part 2/20.txt") 21)) "Part 2 Test 20 Failed")
  (else "All Tests Pass"))