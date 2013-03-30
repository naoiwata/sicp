;; @author naoiwata
;; SICP Chapter2
;; question 2.71

#|
(1) n = 5
      ABCDE(31)
         /\
  ABCD(15) E(16)
        /\
  ABC(7) D(8)
      /\
 AB(3) C(4)
    /\
A(1) B(2)  

(2) n = 10
              ABCDEFGHIJ(1023)
                     /\
        ABCDEFGHI(51) J(512)
                    /\   
       ABCDEFGH(255) I(256)
                  /\
      ABCDEFG(127) H(128)
                /\
      ABCDEF(63) G(64)
              /\
     ABCDE(31) F(32)
            /\
    ABCD(15) E(16)
          /\
    ABC(7) D(8)
        /\
   AB(3) C(4)
    /\
A(1) B(2)  

(3) how may bits are required to encode the most frequent symbol? 
1 bit

(4) how may bits are required to encode the least frequent symbol?
(n - 1) bits
|#

; END