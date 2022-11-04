defmodule PublicKey do
  require Prob

  def generate(p, n, m, pri_k) do
    errors = Prob.beta_m(p, n, m)

    ai = 1..m |> Enum.map(fn _ -> Zp.rand_n(n, p) end)

    bi =
      ai
      |> Enum.map(&Zp.internal_prod(&1, pri_k, p))
      |> Enum.zip_with(errors, fn a, e -> Zp.add(a, e, p) end)

    {ai, bi}
  end
end
