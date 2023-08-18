!     Converts FV1 (record 9) from .fil averaged at nodes to nodal temperature NT11 (record 201)
!
      SUBROUTINE ABQMAIN

      INCLUDE 'aba_param.inc'
      CHARACTER*80 FNAME
      DIMENSION ARRAY(513), JRRAY(NPRECD,513)
      EQUIVALENCE (ARRAY(1),JRRAY(1,1))
	  INTEGER STARTDATE(8), ENDDATE(8)
      
      INTEGER LRUNIT(2,1), JND, KEY, JUNIT, NRU, JRCD, LOUTF, MAXIT, RENAMERES, JSTEP, JINCR, K1
	  DOUBLE PRECISION FIELDVAR, STEPTIME, TOTTIME
	  
	  LOGICAL VERBOSE 
!
!     File initialization
	  VERBOSE = .FALSE. !.TRUE. to output iteration information. Significantly slows processing
      NRU=1
      LRUNIT(1,1)=8
      LRUNIT(2,1)=2
      LOUTF=2
	  WRITE(6,*) '---------------------------'
	  WRITE(6,*) 'ENTER THE FILENAME OF THE .fil TO BE PROCESSED'
      READ(5,"(A)") FNAME
	  
	  CALL DATE_AND_TIME(VALUES = STARTDATE)
      CALL INITPF(FNAME,NRU,LRUNIT,LOUTF)
      JUNIT=8
      CALL DBRNU(JUNIT)
      JND = 0
	  JSTEP = 0
	  JINCR = 0
	  K1    = 0
      
!
!     Loop on all records in results file
	  WRITE(6,*) 'BEGIN PROCESSING'
      DO WHILE (JRCD.NE.1)
!
        CALL DBFILE(0,ARRAY,JRCD)
        IF(JRCD.NE.0) GO TO 110
		K1 = K1+1
		IF (VERBOSE .AND. (JRRAY(1,2).EQ.2000 .OR. JRRAY(1,2).EQ.2001)) THEN
			WRITE(6,"('READING: K1 = ',I16)") K1
			WRITE(6,*) JRRAY(1,1)
			WRITE(6,*) JRRAY(1,2)
			IF (JRRAY(1,2).EQ.201 .OR. JRRAY(1,2).EQ.2000) THEN
				WRITE(6,*) ARRAY(3)
			ELSE
				WRITE(6,*) JRRAY(1,3)
			END IF
			IF (JRRAY(1,2).EQ.201 .OR. JRRAY(1,2).EQ.2000) THEN
				WRITE(6,*) ARRAY(4)
			ELSE
				WRITE(6,*) JRRAY(1,4)
			END IF
			WRITE(6,*) JRRAY(1,5)
			WRITE(6,*) JRRAY(1,6)
		END IF
		
        KEY=JRRAY(1,2)
		
!     Element header record
        IF(KEY.EQ.1) THEN
!          EXTRACT NODE NUMBER: AVERAGED AT NODES
           JND=JRRAY(1,3)
        ELSE IF(KEY.EQ.9) THEN
		   FIELDVAR = ARRAY(3) !Type conversion trickery
           JRRAY(1,1) = 4 !INCREASE RECORD LENGTH
		   JRRAY(1,2) = 201 !NT Type
		   JRRAY(1,3) = JND !NODE
           ARRAY(4) = FIELDVAR !MOVE FV TO ATTR 2: TEMPERATURE. More type conversion trickery
	    ELSE IF(KEY.EQ.2000) THEN
			STEPTIME = ARRAY(4)
			TOTTIME  = ARRAY(3)
			JSTEP = JRRAY(1,8)
			JINCR = JRRAY(1,9)
        END IF
        
!     WRITE RECORD TO NEW FILE
		IF (VERBOSE .AND. (JRRAY(1,2).EQ.2000 .OR. JRRAY(1,2).EQ.2001)) THEN
			WRITE(6,"('WRITING: K1 = ',I16,' | STEP: ',I2, ' | INCR: ',I6)") K1,JSTEP,JINCR
			WRITE(6,*) JRRAY(1,1)
			WRITE(6,*) JRRAY(1,2)
			IF (JRRAY(1,2).EQ.201 .OR. JRRAY(1,2).EQ.2000) THEN
				WRITE(6,*) ARRAY(3)
			ELSE
				WRITE(6,*) JRRAY(1,3)
			END IF
			IF (JRRAY(1,2).EQ.201 .OR. JRRAY(1,2).EQ.2000) THEN
				WRITE(6,*) ARRAY(4)
			ELSE
				WRITE(6,*) JRRAY(1,4)
			END IF
			WRITE(6,*) JRRAY(1,5)
			WRITE(6,*) JRRAY(1,6)
		END IF
        CALL DBFILW(1,ARRAY,JRCD)
		IF(JRCD.EQ.1) THEN
		   WRITE(6,*) "FAILED TO WRITE. TERMINATING..."
		   GO TO 110
	    END IF
!
      END DO
 110  CONTINUE
!
	  WRITE(6,*) 'PROCESSING COMPLETE'
	  WRITE(6,"('PROCESSED ',I16,' LINES')") K1
	  WRITE(6,"('LAST STEP: ',I2,6X,' | INCR: ',I8)") JSTEP,JINCR
	  WRITE(6,"('STEP TIME: ',F8.3, ' | TOTAL TIME: ',F8.3)") STEPTIME,TOTTIME
	  WRITE(6,*) 'CLOSING FILES'
      CLOSE(JUNIT)
      CLOSE(9)
	  WRITE(6,*) 'START:'
	  WRITE(6,140) STARTDATE(1), STARTDATE(2), STARTDATE(3), STARTDATE(5), STARTDATE(6), STARTDATE(7), STARTDATE(8) 
	  CALL DATE_AND_TIME(VALUES = ENDDATE)
	  WRITE(6,*) 'END:'
	  WRITE(6,140) ENDDATE(1), ENDDATE(2), ENDDATE(3), ENDDATE(5), ENDDATE(6), ENDDATE(7), ENDDATE(8) 
!
 140  FORMAT(I4,'-',I2,'-',I2,' ',I2,':',I2,':',I2,'.',I3)
      RETURN
      END
