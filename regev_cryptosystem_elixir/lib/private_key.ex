defmodule PrivateKey do
  require Zp

  def generate(n, p), do: Zp.rand_n(n, p)
end
