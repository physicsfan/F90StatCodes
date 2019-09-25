module message
  implicit none

  private

  public :: msg_list_type
  public :: insert_msg_line
  public :: print_msg_contents
  public :: delete_msg_contents

  integer, parameter :: msg_width = 70

  type :: msg_line_type
     ! a single node in a linked list
     character (len=msg_width) :: line = ""
     type(msg_line_type), pointer :: next => null()
  end type msg_line_type

  type :: msg_list_type
     ! structure to contain the entire list
     logical :: msg_present = .false.
     type(msg_line_type), pointer :: &
          head => null(), &
          tail => null()
  end type msg_list_type

contains

  subroutine insert_msg_line(text_line, msg_list)
    ! inserts a new message line at the end of the list
    implicit none
    character(len=*), intent(in) :: text_line
    type(msg_list_type), intent(inout) :: msg_list
    if(.not. msg_list%msg_present) then
       ! begin a linked list
       allocate(msg_list%head)
       msg_list%tail => msg_list%head
       msg_list%head%line = text_line
       msg_list%msg_present = .true.
    else
       ! add a node to the list
       allocate(msg_list%tail%next)
       msg_list%tail => msg_list%tail%next
       msg_list%tail%line = text_line
    end if
  end subroutine insert_msg_line

  subroutine print_msg_contents(msg_list)
    ! prints the text message lines to standard output
    implicit none
    type(msg_list_type), intent(in) :: msg_list
    type(msg_line_type), pointer :: current_line
    if(msg_list%msg_present) then
       current_line => msg_list%head
       do
          print "(A)", current_line%line
          if(.not. associated(current_line%next)) exit
          current_line => current_line%next
       end do
    end if
  end subroutine print_msg_contents

  subroutine delete_msg_contents(msg_list)
    ! deletes all messages from the list
    implicit none
    type(msg_list_type), intent(inout) :: msg_list
    type(msg_line_type), pointer :: current_line
    if(.not. msg_list%msg_present) return
    do
       current_line => msg_list%head
       msg_list%head => msg_list%head%next
       deallocate(current_line)
       if(.not. associated(msg_list%head)) exit
    end do
    nullify(msg_list%tail)
    msg_list%msg_present = .false.
  end subroutine delete_msg_contents

end module message

program message_test
  use message
  implicit none

  type(msg_list_type) :: msg_list

  call insert_msg_line("Hello, world.", msg_list)
  call print_msg_contents(msg_list)
  call delete_msg_contents(msg_list)
end program message_test
