module FinLib

    open System

    type Formulas () =
        
        /// Simple Interest - Total Interest Earned 
        member this.TIE (p : float , i : float , n : float) =
            let answer = p * i * n
            answer
        /// Simple Interest - Future Amount Due
        member this.FAD (p : float , i : float , n : float) =
            let answer = p + (p * i * n)
            answer
        /// Single Payment - Present Worth
        member this.SP_PW (f : float , i : float , n : float) =
            let answer = (f) * ( (1.0 + i) ** (-n) )
            answer
        /// Single Payment - Future Sum
        member this.SP_FS (p : float , i : float , n : float) =
            let answer = (p) * ( (1.0 + i) ** (n) )
            answer
        /// Effective Annual Interest Rate(in %) - r
        member this.EAIR_R (r : float, m : float) =
            let answer = ((1.0 + (r / m) ) ** m) - 1.0
            answer
        // Effective Annual Interest Rate(in %) - i
        member this.EAIR_I (i : float, m : float) =
            let answer = ((1.0 + i) ** m) - 1.0
            answer
        /// Continuous Compounding - Future Sum
        member this.CC_FS (p : float , r : float , n : float) = 
            let answer = p * (System.Math.Exp(r * n) )
            answer
         /// Continuous Compounding - Present Worth
        member this.CC_PW (f : float , r : float , n : float) = 
            let answer = f * (System.Math.Exp(r * n) )
            answer
        /// Continuous Compounding - Effective Annual Interest Rate(in digits)
        member this.CC_EAIR (r : float) =
            let answer = System.Math.Exp(r) - 1.0
            answer
        /// Uniform Series Compound Amount Factorv - (F/A, i, n) 
        member this.USCAF(a : float, i : float, n : float) =
            let answer = a * ((( (1.0 + i) ** n) - 1.0 ) / i)
            answer
        /// Uniform Series Sinking Fund - (A/F, i, n)
        member this.USSF (f : float, i : float, n : float) =
            let answer = f * (i / (((1.0 + i) ** n) - 1.0))
            answer
        /// Uniform Series Capital Recovery Factor - (A/P, i, n)
        member this.USCRF(p : float, i : float, n : float) =
            let answer = p * ((i * ((1.0 + i) ** n)) / ( ( (1.0 + i) ** n ) - 1.0  ))
            answer
        /// Uniform Series Present Worth Factor - (P/A, i, n)
        member this.USPWF (a : float, i : float, n : float) =
            let answer = a * ((((1.0 + i) ** n) - 1.0)  /  (i * ((1.0 + i) ** n)))
            answer
        /// Arithmetic Gradient Present Worth Factor - (P/G, i, n)
        member this.AGPWF (g : float, i : float, n : float) =
            let answer = g * ((((1.0 + i) ** n) - (i * n) - 1.0)   /    ((i ** 2.0) * ( (1.0 + i) ** n)))
            answer
        /// Arithmetic Gradient Uniform Series Factor - (A/G, i, n)
        member this.AGUSF (g : float, i : float, n : float) =
            let answer = g * ((((1.0 + i) ** n) - (i * n) - 1.0) /   (((i) * ( (1.0 + i) ** n )) - i))
            answer
        /// Geometric Series Present Worth Factor - (P/A, g, i, n)
        member this. GSPWF (a : float, g : float, i : float, n : float) =
            let answer = a * ((1.0 - (((1.0 + g) ** n) * ((1.0 + i) ** -n))) / (i - g))
            answer
        /// Continuous Compounding Sinking Fund - [A/F, r, n]
        member this.CCSF (f : float, r : float, n : float) =
            let answer = f * ((System.Math.Exp(r) - 1.0) / (System.Math.Exp(r * n) - 1.0))
            answer
        /// Continuous Compounding Capital Recovery - [A/P, r, n]
        member this.CCCR(p : float, r : float, n : float) =
            let answer = p * ((System.Math.Exp(r * n) * (System.Math.Exp(r) - 1.0)) / (System.Math.Exp(r * n) - 1.0))
            answer
        /// Continuous Compounding Series Compound Amount - [F/A, r, n]
        member this.CCSCA (a : float, r : float, n : float) =
            let answer = a * ((System.Math.Exp(r * n) - 1.0) / (System.Math.Exp(r) - 1.0))
            answer
        /// Continuous Compounding Series Present Worth - [P/A, r, n]
        member this.CCSPW (a : float, r : float, n : float) =
            let answer = a * ((System.Math.Exp(r * n) - 1.0) / ( System.Math.Exp(r * n) *(System.Math.Exp(r) - 1.0 )   ))
            answer
