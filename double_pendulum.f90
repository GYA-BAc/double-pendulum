program double_pendulum
  implicit none

  real(8), parameter :: pi = 4.d0*atan(1.d0)
  real(8), parameter :: g = 9.8d0
  
  real(8), parameter :: m1 = 3.d0
  real(8), parameter :: l1 = 5.d0

  real(8), parameter :: m2 = 1.d0
  real(8), parameter :: l2 = 2.d0

  ! theta1 theta2 d_theta1 d_theta2
  real(8), dimension(4) :: state = [pi/2.d0, 0.d0, 0.d0, 0.d0]
  real(8), dimension(4, 4) :: k ! RK4
  real(8), dimension(4) :: temp
  
  real(8), parameter :: step = 1.d-3
  real(8) :: c_t = 0

  real(8) :: domain = 10 !s
  
  open(unit=100, file="m1.dat")
  open(unit=200, file="m2.dat")

  do while (c_t < domain)

    k(1,1:2) = state(3:4)
    k(1,3:4) = get_a(state)
    
    temp = state + (0.5d0)*step*k(1,:)

    k(2,1:2) = temp(3:4)
    k(2,3:4) = get_a(temp)

    temp = state + (0.5d0)*step*k(2,:)

    k(3,1:2) = temp(3:4)
    k(3,3:4) = get_a(temp)

    temp = state + step*k(3,:)

    k(4,1:2) = temp(3:4)
    k(4,3:4) = get_a(temp)
    

    state = state + (step/6.d0)*(k(1,:)+2.d0*k(2,:)+2.d0*k(3,:)+k(4,:))

    write(100,*) 0.0d0, 0.0d0
    write(100,*) sin(state(1))*l1, -cos(state(1))*l1
    write(100,*) NEW_LINE('a')

    write(200,*) sin(state(1))*l1, -cos(state(1))*l1
    write(200,*) sin(state(1))*l1+sin(state(2))*l2, -cos(state(1))*l1-cos(state(2))*l2
    write(200,*) NEW_LINE('a')
    c_t = c_t + step
  
  end do

  close(100)
  close(200)

contains

function get_a(r) result(a)
  real(8), dimension(4), intent(in) :: r
  real(8), dimension(2)             :: a
 
  ! equation from https://www.physics.umd.edu/hep/drew/pendulum2.html

  a(1) = (-sin(r(1)-r(2))*(m2*l1*r(3)**2*cos(r(1)-r(2))+m2*l2*r(4)**2) - g*(m1*sin(r(1))-m2*sin(r(2))*cos(r(1)-r(2)))) / (l1*(m1+m2*sin(r(1)-r(2))**2))

  a(2) = (sin(r(1)-r(2))*(m1*l1*r(3)**2+m2*l2*r(4)**2*cos(r(1)-r(2))) + g*(m1*sin(r(1))*cos(r(1)-r(2))-m1*sin(r(2)))) / (l2*(m1+m2*sin(r(1)-r(2))**2))

end function


end program
