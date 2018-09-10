PROGRAM ListOptions
  IMPLICIT NONE

  TYPE state
     INTEGER :: ps
     INTEGER :: qs
     INTEGER :: trn
     LOGICAL :: pw
     LOGICAL :: qw
  END TYPE state

  TYPE(state), DIMENSION(288,3) :: States  
  LOGICAL :: pwants = .FALSE., qwants = .FALSE., ValidChange
  INTEGER :: p = 0, q = 0, turn = 1, i, j, k, l, cnt = 0
      
  Do i=0,5
     p = p+1
     q = 0
     IF (ValidChange(p,0,turn)) THEN
        DO j=0,5
           q = q+1
           IF (ValidChange(0,q,turn)) THEN
              DO k=1,8
                 cnt = cnt + 1
                 States(cnt,1)%ps = p
                 States(cnt,1)%qs = q
                 States(cnt,1)%trn = turn
                 States(cnt,1)%pw = pwants
                 States(cnt,1)%qw = qwants
                 SELECT CASE (MOD(k,2))
                 CASE (0)
                    pwants = .NOT. pwants
                 CASE (1)
                    qwants = .NOT. qwants
                 END SELECT
                 IF (0 == MOD(k,4)) THEN
                    IF (turn == 1) THEN
                       turn = 2
                    ELSE
                       turn = 1
                    END IF
                 END IF
              END DO
           END IF
        END DO
     END IF
  END DO

  DO l=1,cnt
     PRINT "('(',1I1,2I2,2L2,')')", States(l,1)
  END DO
  PRINT *, cnt
END PROGRAM ListOptions


PURE FUNCTION ValidChange(p_cur,q_cur,t_cur) RESULT(isValid)
  INTEGER, intent(in)  :: p_cur,q_cur,t_cur
  LOGICAL :: isValid

  IF (((p_cur == 6) .AND. (t_cur == 2)) &
       .OR. ((q_cur == 6) .AND. (t_cur == 1))) THEN
     isValid = .FALSE.
     RETURN
  ELSE
     isValid = .TRUE.
     RETURN
  END IF
END FUNCTION ValidChange


!!$SUBPROCESS StateBranch (States,row) RESULT()
!!$  INTEGER, intent(in) :: row
!!$  TYPE(States), intent(in) :: States
!!$END FUNCTION StateBranch
