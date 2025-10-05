! ============================
! mystats.f90
! Module containing all stat-related operations.
! ============================

MODULE StatsModule
  IMPLICIT NONE

  INTEGER, PARAMETER :: max_n = 100
  INTEGER :: N
  REAL :: NUM(max_n)
  REAL :: AVG, STD

  COMMON /DATA_BLOCK/ N, NUM, AVG, STD

CONTAINS

  SUBROUTINE InitData()
    INTEGER :: I
    N = 0
    AVG = 0.0
    STD = 0.0
    DO I = 1, max_n
      NUM(I) = 0.0
    END DO
  END SUBROUTINE InitData

  SUBROUTINE ReadInput(filename, ios)
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT)     :: ios
    INTEGER :: i
    OPEN(UNIT=10, FILE=filename, STATUS='OLD', IOSTAT=ios)
    IF (ios /= 0) RETURN

    READ(10, *, IOSTAT=ios) N
    IF (ios /= 0 .OR. N > max_n) THEN
      ios = 1
      RETURN
    END IF

    READ(10, *, IOSTAT=ios) (NUM(i), i = 1, N)
    CLOSE(10)
  END SUBROUTINE ReadInput

  SUBROUTINE ComputeAll(ierr)
    INTEGER, INTENT(OUT) :: ierr
    REAL :: sum, var, diff
    INTEGER :: i

    IF (N <= 0) THEN
      ierr = 1
      RETURN
    END IF

    sum = 0.0
    DO i = 1, N
      sum = sum + NUM(i)
    END DO
    AVG = sum / N

    var = 0.0
    DO i = 1, N
      diff = NUM(i) - AVG
      var = var + diff * diff
    END DO

    STD = SQRT(var / N)
    ierr = 0
  END SUBROUTINE ComputeAll

  FUNCTION FindMax(n) RESULT(maxval)
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    REAL :: maxval

    maxval = NUM(1)
    DO i = 2, n
      IF (NUM(i) > maxval) THEN
        maxval = NUM(i)
      END IF
    END DO
  END FUNCTION FindMax

  SUBROUTINE NormalizeData(n)
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    DO i = 1, n
      NUM(i) = (NUM(i) - AVG) / STD
    END DO
  END SUBROUTINE NormalizeData

  SUBROUTINE WriteOutput(filename, maxval, variance)
    CHARACTER(*), INTENT(IN) :: filename
    REAL, INTENT(IN) :: maxval, variance

    OPEN(UNIT=20, FILE=filename, STATUS='UNKNOWN')
    WRITE(20, 100) N, AVG, STD, maxval, variance
    CLOSE(20)

100 FORMAT('Number of values: ', I3, /, &
           'Average: ', F8.3, /, &
           'Standard Deviation: ', F8.3, /, &
           'Maximum Value: ', F8.3, /, &
           'Variance: ', F8.3)
  END SUBROUTINE WriteOutput

  SUBROUTINE WriteNormalized(filename)
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER :: i

    OPEN(UNIT=21, FILE=filename, POSITION='APPEND')
    WRITE(21, *) 'Normalized Values:'
    DO i = 1, N
      WRITE(21, '(F8.4)') NUM(i)
    END DO
    CLOSE(21)
  END SUBROUTINE WriteNormalized

END MODULE StatsModule
