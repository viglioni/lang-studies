defmodule Prob do
  import Statistics.Distributions.Normal, only: [rand: 2]

  defp calc_beta(n), do: 1 / (:math.sqrt(n) * :math.log(n) ** 2)

  defp std_dev(n), do: calc_beta(n) / :math.sqrt(2 * :math.pi())

  def beta(p, n), do: (p * rand(0, std_dev(n))) |> round |> rem(p)

  def beta_m(p, n, m), do: 1..m |> Enum.map(fn _ -> beta(p, n) end)

  def bool_rand(_), do: [true, false] |> Enum.at(:rand.uniform(2) - 1)

  def choose_sub_set(m) do
    res = 1..m |> Enum.map(&bool_rand/1)

    if Enum.any?(res) do
      res
    else
      choose_sub_set(m)
    end
  end
end
