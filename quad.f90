!This is a file containing Fortran code that solves the quadratic equation.  
!You may notice already that Fortran uses a different character to indicate
!comments than Python (Fortran uses ! instead of #).
!
!By convention, files containing Fortran code have one of these suffixes:
!  .f, .for, .f90, .f95
!This is Fortran's version of Python's ".py" convention.  The .f90 and
!.f95 indicate which Fortran standard the code in the file assumes.
!That's a bit beyond the scope of this course, so suffice it to say we use
!the Fortran 90 standard in this file.
!
!The purpose of this code is to demonstrate some of the Fortran basics.  To
!execute it in a shell you would do
!  $ gfortran quad.f90
!This will compile the code into an executable file the computer understands.
!The file will be called a.out.  If you do an `ls`, you should see it.  To 
!execute a.out enter the following into the shell and press enter:
!  $ ./a.out
!The result, if you haven't made any changes, will be
!  The solution to a x^2 + b^x + c = 0, with
!  a =    1.00000000    
!  b =    4.00000000    
!  c =    2.00000000    
!  is: 
!  root x1:  -0.585786462    
!  root x2:   -3.41421366   
!  Would you like to solve another quadratic equation?
!     Please type (y)es or (n)o:
!  
!Type 'n' and press enter to end the program.
!
!Let's get to the code already!

!The `program <name>` and `end program` statements tell the compiler where the
!beginning and end of your Fortran code is. 
program quad
implicit none  !For historical reasons, ALL good Fortran programs have this
               !`implicit none` statement immediately after the `program <name>`
               !statement.

   !Below, we declare variables.  Unlike in Python, which guesses what type you
   !want your variable to be based on context, in Fortran we must explicitly 
   !declare our variable types.  Some common types are:
   !  integer    --> For integer numbers
   !  real       --> For decimal (rational) numbers
   !  character  --> For text
   !  logical    --> To represent binary states, either .true. or .false.
   real :: x1, x2           ! These hold the roots of the quadratic
   logical :: do_another    ! A logical storing the user's preference
   integer :: i             ! A loop index
   !In Python we had lists.  Fortran has arrays.  Here we define an array to
   !store the three coefficients used in the quadratic equation:
   real :: coeff(3)
   !Next we define what in some languages is called a "string," known as a
   !character array in Fortran.  We have to specify the maximum number of
   !characters this variable can hold.  We've chosen 64 here.
   character(len=64) :: user_prompt

   !Now that we've declared our variables we'll initialize some of them. Notice
   !that unlike Python, Fortran array indexes start at 1, *not* 0.
   coeff(1) = 1.0
   coeff(2) = 4.0
   coeff(3) = 2.0
   user_prompt = 'The solution to a x^2 + b^x + c = 0, with'

   !Use Fortran's `print` statement to tell the user what we're doing
   print *, user_prompt
   print *, '  a = ', coeff(1)
   print *, '  b = ', coeff(2)
   print *, '  c = ', coeff(3)
   print *, '  is: '

   !Call a function to solve the quadratic equation with a given set of
   !coefficients stored in the array coeff.  x1 and x2 store the result.
   call solve(coeff, x1, x2)

   !Print out the result
   print *, '  root x1: ', x1
   print *, '  root x2: ', x2

   !Now let's interact with the user, to see if they want to try a different set
   !of coefficients, and if so, which ones?
  
   !To do this we make use of the function solve_another() in what is called a
   !while loop.  A while loop executes the body of the loop as long as a
   !condition is satisfied.
   do_another = solve_another()
   do while (do_another)
      !Get a new set of coefficients from the user
      call get_coeff(coeff)
   
      !Use Fortran's `print` statement to tell the user what we're doing
      print *, user_prompt
      print *, '  a = ', coeff(1)
      print *, '  b = ', coeff(2)
      print *, '  c = ', coeff(3)
      print *, '  is: '

      !Call a function to solve the quadratic equation
      call solve(coeff, x1, x2)

      !Print out the result
      print *, '  root x1: ', x1
      print *, '  root x2: ', x2

      !Ask if the user wants to continue
      do_another = solve_another()
   end do

   print *, 'Math is fun, huh?  Hope to see you again soon!  Exiting...'

!The above code is what we want Fortran to execute.  However, our program also
!defines functions.  We indicate these in the `contains` region of the program.
contains

   !The most common type of function in Fortran is a subroutine. The arguments
   !to a subroutine (in this case, coeff, x1, and x2) can both send
   !information to and get information from a subroutine.  In this case, our
   !input argument is the array coeff while our output arguments are x1 and x2.
   !We indicate this with the `intent` attribute, as you will see.
   subroutine solve(coeff, x1, x2)
      !We have to to declare the variable types in the function.
      real, intent(in ), dimension(3) ::  coeff
      real, intent(out)               ::  x1, x2
      
      !We can declare variables only visible to this subroutine as well:
      real :: a, b, c

      !For convenience and readability, store the coefficients in individual
      !real variables
      a = coeff(1)
      b = coeff(2)
      c = coeff(3)

      !Set x1 and x2 to the two solutions to the quadratic equation
      x1 = (-b + sqrt(b**2 - 4.0 * a * c)) / (2.0 * a)
      x2 = (-b - sqrt(b**2 - 4.0 * a * c)) / (2.0 * a)
   end subroutine

   !Fortran can also handle functions that return a result instead of using the
   !arguments passed to it for returning results.  Here's one that returns a
   !logical variable representing the user's response to a question
   function solve_another() result(answer)
      !We have to tell Fortran what type of variable the result is
      logical :: answer
      character(len=1) :: response

      !Ask the user if they would like to solve another quadratic
      print *, 'Would you like to solve another quadratic equation?'
      print *, '   Please type (y)es or (n)o: '

      !To get the user's response, we use Fortran's read statement
      !This will read what the user types into the terminal and store it in the
      !`response` variable.
      read(*,*) response

      !Now we want to transform the user's character response into a logical,
      !making use of if statements.  We also use the logical operator .or.,
      !which evaluates to true if either statement is true.  Otherwise, it
      !evaluates to false
      if (response == 'y' .or. response == 'Y') then
         answer = .true.
      else if (response == 'n' .or. response == 'N') then
         answer = .false.
      else
         print *, 'You responded with: ', response
         print *, 'This is not a valid response, exiting!'
         stop 'ERROR!'
      endif
   end function 

   !This subroutine gets a new set of coefficients from the user
   subroutine get_coeff(coeff)
      real, dimension(3), intent(out) :: coeff
      integer :: i
      real :: cur_coeff

      !Here we see our first do loop.  This is similar to Python's for loop.
      !This loop executes 3 times.  The first time, i will be 1, the next 2,
      !and then finally 3.
      do i = 1, 3
         print *, 'Please provide coefficient # ', i
         read(*,*) cur_coeff
         coeff(i) = cur_coeff
      enddo
   end subroutine
end program

!Fortran exercises:
!1) If you haven't yet, compile and execute this code.  Does it behave as you expect?
!2) Write a Fortran version of all the programs you wrote for the Python
!tutorial.  Specifically, write the following Fortran programs:
!  -A program that includes if, else if, and else statements.  Verify that all
!   possible branches of the if structure execute when appropriate.
!  -A program with at least one real array.  Fortran has no tuple, so no need
!   to try to make one.  Print the values of the real array out, change at least
!   one value, and then print again.
!  -A program with a real array.  First, initialize the array with some numbers.
!   Then, use a do loop to make a real array containing
!   the same values multiplied by 2.  Be sure to print the results so we can
!   verify the arrays contain what we expect.
