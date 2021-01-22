      integer function uscore(file)
c
c   routine to find the position of the first underscore ('_')
c   character in a character variable called file
c
c   len is a UNIX Ridge function to find the length of a character variable
c   then leng defines the maximum value of uscore
c
      character*(*) file
      integer ounit
      common /innout/ inunit,ounit
      leng=len(file)
      do 1 i=1,leng
         if(file(i:i).ne.'_') go to 1
            uscore=i
            return
    1 continue
c  
c   if no '_' is found, set uscore = leng   
c 
      uscore=leng
      return
      end
