program simpsons
    real*8 a,b,approx
    integer*4 i,nx
    real*8 simpsons38
    a=0.0
    b=1.0
    write(6,*)' n simpsons 3/8 rule'
    nx=3
    do i=1,12
        approx=simpsons38(a,b,nx)
        write(6,*)nx,approx
        nx=3*nx
    end do
    stop
    end
    function f(x)
        real*8 x,f
        f=exp(-x/2.0)
        return
        end
        real*8 function simpsons38(a,b,nx)
        integer*4 i,nx
        real*8 a,b,f,h,xa,fa
        h=(b-a)/dfloat(nx)
        sum=(f(a)+f(b))
        do i=1,nx-1
            xa=a+dfloat(i)*h
            fa=f(xa)
            if(mod(i,3).eq.0)then
                sum=sum+2.0*fa
                else
                    sum=sum+3.0*fa
            end if
        end do
        simpsons38=(3.0*h/8.0)*sum
        return
end
