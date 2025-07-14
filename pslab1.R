# question 1 

A = c(5,10,15,20,25,30)


A

min(A)
max(A)

# question 2 

num = as.integer(readline(prompt = "Enter number:"))
fact = 1
if (fact<0) {
	print("Factorial is negative.")
} else if(num==0) {
	print("Factorial of 0 is 1")
} else {
	for(i in 1:num) {
		fact=fact*i
	}
	cat("factorial of num is:" , fact)
	}


# question 3 

nterms = 15
n1 = 0
n2 = 1
count = 2
if(nterms <= 0) {
	print("Enter positive integer")
} else{
	if(nterms == 1) {
		print("Fibonacci sequence is:")
		print(n1)
	} else {
		print("Fibonacci sequence:")
		print(n1)
		print(n2)
		while(count < nterms) {
			n3=n2+n1
			print(n3)
			n1=n2
			n2=n3
			coutn=count+1
		}
	}
}

#question 4

# Ask the user to enter two numbers
num1 <- as.numeric(readline(prompt = "Enter first number: "))
num2 <- as.numeric(readline(prompt = "Enter second number: "))

# Ask the user to choose an operation
cat("Choose an operation:\n")
cat("1. Addition (+)\n")
cat("2. Subtraction (-)\n")
cat("3. Multiplication (*)\n")
cat("4. Division (/)\n")
choice <- as.integer(readline(prompt = "Enter your choice (1/2/3/4): "))

# Perform the operation based on user's choice
if(choice == 1){
  result <- num1 + num2
  cat("Result:", result)
} else if(choice == 2){
  result <- num1 - num2
  cat("Result:", result)
} else if(choice == 3){
  result <- num1 * num2
  cat("Result:", result)} else if(choice == 4){
  if(num2 == 0){
    cat("Error: Cannot divide by zero")
  } else {
    result <- num1 / num2
    cat("Result:", result)
  }
} else {
  cat("Invalid choice! Please enter 1, 2, 3, or 4.")
}




YT3K-5TXN-9YYG-BCH0
steam activation key assassins creed revelation 

