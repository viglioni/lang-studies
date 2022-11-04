defmodule Zp do
  def elements(p), do: Enum.to_list(0..(p - 1))

  defp abs(num, p), do: (num + p) |> rem(p)

  def zp(num, p), do: num |> rem(p) |> abs(p)

  def rand(p), do: :rand.uniform(p)

  def rand_n(n, p), do: 1..n |> Enum.map(fn _ -> rand(p) end)

  def multiply(a, b, p), do: (a * b) |> zp(p)

  def add(a, b, p), do: (a + b) |> zp(p)

  def subtract(a, b, p), do: (a - b) |> zp(p)

  def add_vec(v1, v2, p), do: Enum.zip_with(v1, v2, &add(&1, &2, p))

  def sum_vector(vec, p), do: vec |> Enum.sum() |> zp(p)

  def sum_vectors(vectors, p) do
    vectors |> Enum.reduce(&add_vec(&1, &2, p))
  end

  def internal_prod(v1, v2, p), do: Enum.zip_with(v1, v2, &multiply(&1, &2, p)) |> sum_vector(p)
end
