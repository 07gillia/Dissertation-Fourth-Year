# My first program in R Programming

myString = "Hello, World!"

matrix = matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE)

array = array(c('green','yellow'),dim = c(6,6,2))

#print(myString)
#print(matrix)
#print(array)

# Create a vector.
apple_colors = c('green','green','yellow','red','red','red','green')

# Create a factor object.
factor_apple = factor(apple_colors)

# Print the factor.
#print("the factor apple")
#print(factor_apple)

#print("nlevels(factor_apple)")
#print(nlevels(factor_apple))

if(FALSE){
"this is what a significant comment looks like?"
}


# Create the data frame.
BMI = data.frame(
   gender = c("Male", "Male","Female"), 
   height = c(152, 171.5, 165), 
   weight = c(81,93, 78),
   Age = c(42,38,26)
)

#print(BMI)

#print(ls())

v = 1:100

#print(v)
#print(v[39])

sum = function(arg_1, arg_2, arg_3) {
	return (arg_1 + arg_2 + arg_3)
}

#print(sum(1,5,9))

w = mean(0:1200)

#print(w)

# create a function that prints squares of numbers in sequence
# we can create a function that does the same thing with no arguments up to a hardcoded number
# we can also give default values
maths.list_squares = function(a) {
	for(i in 1:a) {
		b = i^2
		print(b)
	}
}

#maths.list_squares(20)

maths.list_squares_default = function(a = 10) {
	for(i in 1:a) {
		b = i^2
		print(b)
	}
}

maths.list_squares_default()
maths.list_squares_default(15)

for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

# a for loop