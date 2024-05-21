PROGRAM tarefa1
  INTEGER :: rule(6), i

  rule = (/ 232, 254, 51, 73, 105, 129 /)

  DO i=1, 6
    CALL CA(rule(i), "0", "0-rule=")
    CALL CA(rule(i), "1", "1-rule=")
    CALL CA(rule(i), "r", "R-rule=")
  END DO

  CONTAINS
  SUBROUTINE CA(rule_number, initial_state, file_prefix)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: rule_number
    CHARACTER(*), INTENT(IN) :: initial_state, file_prefix

    INTEGER, PARAMETER :: L = 100
    CHARACTER(1) :: state(L), new_state(L)
    CHARACTER(3) :: trinca
    CHARACTER(8) :: rule
    CHARACTER(14) :: filename
    INTEGER :: i, j, index

    WRITE(rule, '(B8.8)') rule_number
    WRITE(filename, '( A7, I3.3, ".out" )') file_prefix, rule_number

    OPEN(UNIT=1,FILE=filename,STATUS="UNKNOWN")
    CALL SRAND(42)

    IF (initial_state == '0') THEN
      state = '0'
    ELSE IF (initial_state == '1') THEN
      state = '1'
    ELSE
      DO i=1, L
        write(state(i), '(B1.1)') nint(rand())
      END DO
    END IF

    WRITE(1,*) state
    DO j=1, L-1
      DO i=1, L
        trinca = state(MOD(i-2+L,L)+1) // state(i) // state(MOD(i,L)+1)
        READ(trinca, '(B3)') index
        index = 8 - index
        new_state(i) = rule(index:index)
      END DO
      WRITE(1,*) new_state
      state = new_state
    END DO
  END SUBROUTINE
END PROGRAM
