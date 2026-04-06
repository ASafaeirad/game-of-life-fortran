module terminal_mod
   use ffi
   use kinds_mod

   implicit none
   private
   public :: clear_screen, poll_keyboard_during_timeout
   public :: keyboard_action_none, keyboard_action_reset, keyboard_action_quit

   integer(rk), parameter :: keyboard_action_none = 0
   integer(rk), parameter :: keyboard_action_reset = 1
   integer(rk), parameter :: keyboard_action_quit = 2
contains

   subroutine clear_screen()
      implicit none
      character(len=*), parameter :: clear = char(27)//"[2J"//char(27)//"[H"

      print *, clear
   end subroutine clear_screen

   subroutine poll_keyboard_during_timeout(timeout, poll_interval, action)
      integer(rk), intent(in) :: timeout, poll_interval
      integer(rk), intent(out) :: action

      integer(rk) :: remaining_sleep, sleep_chunk

      action = keyboard_action_none
      remaining_sleep = timeout

      do while (remaining_sleep > 0)
         sleep_chunk = min(poll_interval, remaining_sleep)
         call usleep(sleep_chunk)
         remaining_sleep = remaining_sleep - sleep_chunk

         call process_keyboard_input(poll_interval, action)
         if (action /= keyboard_action_none) exit
      end do
   end subroutine poll_keyboard_during_timeout

   subroutine process_keyboard_input(poll_interval, action)
      integer(rk), intent(in) :: poll_interval
      integer(rk), intent(out) :: action

      character(len=1) :: key
      logical :: has_key

      action = keyboard_action_none
      call read_key_nonblocking(key, has_key)

      if (.not. has_key) return

      select case (key)
       case ('r')
         action = keyboard_action_reset
       case ('q')
         action = keyboard_action_quit
       case (' ')
         call pause_until_resume(poll_interval)
      end select
   end subroutine process_keyboard_input

   subroutine pause_until_resume(poll_interval)
      integer(rk), intent(in) :: poll_interval

      character(len=1) :: key
      logical :: has_key

      print '(A)', 'Paused. press "space" to resume.'
      do
         call read_key_nonblocking(key, has_key)
         if (has_key .and. key == ' ') then
            print '(A)', 'resuming...'
            exit
         end if
         call usleep(poll_interval)
      end do
   end subroutine pause_until_resume


end module terminal_mod
