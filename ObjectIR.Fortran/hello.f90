program hello
    implicit none
    
    call greet()
    call greet()
end program hello

subroutine greet()
    print *, "Hello from subroutine!"
end subroutine greet
