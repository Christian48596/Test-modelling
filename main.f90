! ============================
! main.f90
! Main program to read data from a file, calculate statistics,
! and print results using StatsModule and ExtraMath.
!
! To compile:
!   gfortran -o stats main.f90 mystats.f90 extras.f90
! To run:
!   ./stats
! ============================

PROGRAM FileIOStats
  USE StatsModule
  USE ExtraMath
  IMPLICIT NONE

  INTEGER :: ios, ierr
  CHARACTER(LEN=100) :: infile, outfile
  REAL :: variance, maxval

  infile  = 'input.dat'
  outfile = 'output.dat'

  CALL InitData()

  CALL ReadInput(infile, ios)
  IF (ios /= 0) THEN
    PRINT *, 'Error reading input file.'
    STOP
  END IF

  CALL ComputeAll(ierr)
  IF (ierr /= 0) THEN
    PRINT *, 'Error in computation.'
    STOP
  END IF

  maxval = FindMax(N)
  variance = Square(STD)

  CALL WriteOutput(outfile, maxval, variance)

  CALL NormalizeData(N)
  CALL WriteNormalized(outfile)

  PRINT *, 'Results written to ', outfile
END PROGRAM FileIOStats

