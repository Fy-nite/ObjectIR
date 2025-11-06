program main
    implicit none
    
    call greet()
    call greet()
end program main

subroutine greet()
    print *, "Hello from subroutine!"
end subroutine greet
