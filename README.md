<h1>A minimalistic functional language</h1>
<p>
    This is a simple interpreter for a minimal functional language as seen in 
    many computability and &lambda;-calculus classes. Syntax is:
    <ul>
        <li>
            <code>Z, S, P(n, i)</code> for zero function, successor and 
            projection of i over n params.
        </li>
        <li>
            <code>C[F, G1, G2, ...]</code> for composition of 
            F(G1(...), G2(...), ...)
        </li>
        <li>
            <code>R[K, G]</code> for recursion. Where K is the function to apply
            to base case and G on recursive case.
        </li>
        <li>
            <code>M[G]</code> minimization of G.
        </li>
    </ul>
    Params needs to be passed as: <code>(p1, p2, p3, ...)</code> or you will 
    encouter problems.
</p>
