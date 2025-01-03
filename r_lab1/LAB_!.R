#EXERCISE_1

# '/' gives quotient
2.7/2

#integer quotient(integer division)
2.7%/%2

# '%%' gives remainder
2.7 %% 2

#dividing imaginary number 
10+5i/2

#rounds to nearest integer
round(2.5)
round(-2.5)

#2%/%4=0 and 0-1 =-1
2%/%4-1

#exponent '** / ^' , multpli '*'
3*2**2
3*2^2
3**2*2

7%/%4
7%%4
-7%%4

#no rounding just truncate decimal place numbers
trunc(5.7)
trunc(-5.7)


#EXERCISE_2

#function for ceiling operation func
my_ceiling<-function(x){ trunc(x)+1 }
my_ceiling(5.3)


#EXERCISE_3
a=1;b=2;c=4

#gives 'True' as both a and b are non-zero
a&b

# !(a<b) is 'False' , (c>b) is 'True', in this case 'OR' gives True
!(a<b) | (c>b)


#EXERCISE_4

#assigns a vector(numbers) to a variable 'x'
x=c(5,3,7,8)
x

#default data type for number vector is 'numeric' in R, not integer type
is.integer(x)
is.numeric(x)

#gives error
#x=integer(x)
#x

#converts numeric vector into integer vector
x=as.integer(x)
x

#gives True as it already converted to integer type
x=is.integer(x)
x


#EXERCISE_5

x=sqrt(2)

#gives 'False' becuz real numbers is stored in binary approximations, not all decimal values can be represented exactly in binary
#so x*x is slightly less than 2 (rounding errors) 
x*x==2

#gives how much rounding error occurred
x**x-2

# 'all.equal()' used specially used for comparing real number
all.equal(x*x,2)


#EXERCISE_6

#generates sequences start from 0 to 10
0:10

#generates sequence of numbers in reverse order
15:5

#gives sequences of numbers start from 0 to 1.5 with 0.1 step size
#same as linspace() function in numpy python
seq(0,1.5,0.1)

#gives in reverse
#step size is negative as we have decremented
seq(6,4,-0.2)

