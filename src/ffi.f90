module ffi
   use iso_c_binding

   implicit none

   private
   public :: usleep, enable_raw_input, disable_raw_input, read_key_nonblocking

   integer(c_int), parameter :: stdin_fd = 0_c_int
   integer(c_int), parameter :: tcsanow = 0_c_int
   integer(c_int), parameter :: icanon_flag = 2_c_int
   integer(c_int), parameter :: echo_flag = 8_c_int

   integer(c_int), parameter :: vtime_index = 6_c_int
   integer(c_int), parameter :: vmin_index = 7_c_int

   type, bind(C) :: c_termios
      integer(c_int) :: c_iflag
      integer(c_int) :: c_oflag
      integer(c_int) :: c_cflag
      integer(c_int) :: c_lflag
      character(kind=c_char) :: c_line
      character(kind=c_char) :: c_cc(32)
      integer(c_int) :: c_ispeed
      integer(c_int) :: c_ospeed
   end type c_termios

   type(c_termios), save :: saved_termios
   logical, save :: raw_mode_enabled = .false.

   interface
      subroutine usleep(microseconds) bind(C)
         import :: c_int
         integer(c_int), value :: microseconds
      end subroutine usleep

      function c_read(fd, buf, count) bind(C, name='read') result(bytes_read)
         import :: c_int, c_ptr, c_size_t, c_long
         integer(c_int), value :: fd
         type(c_ptr), value :: buf
         integer(c_size_t), value :: count
         integer(c_long) :: bytes_read
      end function c_read

      function tcgetattr(fd, termios_p) bind(C, name='tcgetattr') result(status)
         import :: c_int, c_termios
         integer(c_int), value :: fd
         type(c_termios), intent(out) :: termios_p
         integer(c_int) :: status
      end function tcgetattr

      function tcsetattr(fd, optional_actions, termios_p) bind(C, name='tcsetattr') result(status)
         import :: c_int, c_termios
         integer(c_int), value :: fd
         integer(c_int), value :: optional_actions
         type(c_termios), intent(in) :: termios_p
         integer(c_int) :: status
      end function tcsetattr
   end interface

contains

   subroutine enable_raw_input()
      type(c_termios) :: raw_termios

      if (raw_mode_enabled) return

      if (tcgetattr(stdin_fd, saved_termios) /= 0_c_int) return ! Failed to get terminal attributes

      raw_termios = saved_termios
      raw_termios%c_lflag = iand(raw_termios%c_lflag, not(ior(icanon_flag, echo_flag))) ! Disable canonical mode and echo
      raw_termios%c_cc(vtime_index) = achar(0, kind=c_char)
      raw_termios%c_cc(vmin_index) = achar(0, kind=c_char)

      if (tcsetattr(stdin_fd, tcsanow, raw_termios) == 0_c_int) then
         raw_mode_enabled = .true.
      end if
   end subroutine enable_raw_input

   subroutine disable_raw_input()
      if (.not. raw_mode_enabled) return

      if (tcsetattr(stdin_fd, tcsanow, saved_termios) == 0_c_int) then
         raw_mode_enabled = .false.
      end if
   end subroutine disable_raw_input

   subroutine read_key_nonblocking(key, has_key)
      character(len=1), intent(out) :: key
      logical, intent(out) :: has_key

      character(kind=c_char, len=1), target :: c_key
      integer(c_long) :: bytes_read

      key = ' '
      has_key = .false.

      if (.not. raw_mode_enabled) return

      bytes_read = c_read(stdin_fd, c_loc(c_key), 1_c_size_t)
      if (bytes_read == 1_c_long) then
         key = achar(ichar(c_key))
         has_key = .true.
      end if
   end subroutine read_key_nonblocking
end module ffi
