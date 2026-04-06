module terminal_mod
   implicit none
   private
   public :: clear_screen
contains

   subroutine clear_screen()
      implicit none
      character(len=*), parameter :: clear = char(27)//"[2J"//char(27)//"[H"

      print *, clear
   end subroutine clear_screen
end module terminal_mod
