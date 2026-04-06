module ffi
   interface usleep
      subroutine usleep(microseconds) bind(C)
         use iso_c_binding, only: c_int
         integer(c_int), value :: microseconds
      end subroutine usleep
   end interface usleep
end module ffi
