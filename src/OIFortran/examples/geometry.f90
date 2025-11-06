! Simple Fortran 90 module with derived type and functions
! This demonstrates basic module structure, derived types, and functions
! Filename: geometry.f90

module geometry
    implicit none
    private
    
    ! Public interface
    public :: point, distance, magnitude, translate
    
    ! Derived type definition
    type :: point
        real :: x, y, z
    end type point
    
contains
    
    ! Calculate distance between two points
    real function distance(p1, p2)
        type(point), intent(in) :: p1, p2
        real :: dx, dy, dz
        
        dx = p2%x - p1%x
        dy = p2%y - p1%y
        dz = p2%z - p1%z
        
        distance = sqrt(dx**2 + dy**2 + dz**2)
    end function distance
    
    ! Calculate magnitude (distance from origin)
    real function magnitude(p)
        type(point), intent(in) :: p
        magnitude = sqrt(p%x**2 + p%y**2 + p%z**2)
    end function magnitude
    
    ! Translate a point by a vector
    type(point) function translate(p, offset)
        type(point), intent(in) :: p, offset
        translate%x = p%x + offset%x
        translate%y = p%y + offset%y
        translate%z = p%z + offset%z
    end function translate
    
end module geometry

! Main program using the geometry module
program geometry_demo
    use geometry
    implicit none
    
    type(point) :: p1, p2, p3
    real :: dist, mag
    
    ! Create two points
    p1%x = 1.0
    p1%y = 2.0
    p1%z = 3.0
    
    p2%x = 4.0
    p2%y = 5.0
    p2%z = 6.0
    
    ! Calculate distance
    dist = distance(p1, p2)
    print *, "Distance between p1 and p2:", dist
    
    ! Calculate magnitude of p1
    mag = magnitude(p1)
    print *, "Magnitude of p1:", mag
    
    ! Translate p1
    p3 = translate(p1, p2)
    print *, "Translated point:", p3%x, p3%y, p3%z
    
end program geometry_demo
