! Fortran 90 Function Parameters with INTENT
! Demonstrates INTENT(IN), INTENT(OUT), INTENT(INOUT)
! Filename: function_parameters.f90

module vector_operations
    implicit none
    private
    public :: vector_type, normalize, dot_product, cross_product
    
    type :: vector_type
        real :: x, y, z
    end type vector_type
    
contains
    
    ! Normalize a vector (INOUT: modified in place)
    subroutine normalize(v)
        type(vector_type), intent(inout) :: v
        real :: length
        
        length = sqrt(v%x**2 + v%y**2 + v%z**2)
        
        if (length > 0.0) then
            v%x = v%x / length
            v%y = v%y / length
            v%z = v%z / length
        end if
    end subroutine normalize
    
    ! Calculate dot product (IN: both inputs unchanged, OUT: result)
    real function dot_product(v1, v2)
        type(vector_type), intent(in) :: v1, v2
        
        dot_product = v1%x * v2%x + v1%y * v2%y + v1%z * v2%z
    end function dot_product
    
    ! Calculate cross product (IN: inputs, OUT: result)
    type(vector_type) function cross_product(v1, v2)
        type(vector_type), intent(in) :: v1, v2
        
        cross_product%x = v1%y * v2%z - v1%z * v2%y
        cross_product%y = v1%z * v2%x - v1%x * v2%z
        cross_product%z = v1%x * v2%y - v1%y * v2%x
    end function cross_product
    
end module vector_operations

! Main program
program vector_demo
    use vector_operations
    implicit none
    
    type(vector_type) :: v1, v2, v3
    real :: dp, length
    
    ! Initialize vectors
    v1%x = 1.0
    v1%y = 0.0
    v1%z = 0.0
    
    v2%x = 0.0
    v2%y = 1.0
    v2%z = 0.0
    
    ! Dot product
    dp = dot_product(v1, v2)
    print *, "Dot product (v1 . v2):", dp
    
    ! Cross product
    v3 = cross_product(v1, v2)
    print *, "Cross product (v1 x v2):", v3%x, v3%y, v3%z
    
    ! Length before normalization
    length = sqrt(v3%x**2 + v3%y**2 + v3%z**2)
    print *, "Length of cross product:", length
    
    ! Normalize
    call normalize(v3)
    print *, "Normalized cross product:", v3%x, v3%y, v3%z
    
end program vector_demo
