defmodule Regev do
  import Statistics.Distributions.Normal, only: [rand: 0]
  require Prob
  require Zp
  require PrivateKey
  require PublicKey
  require Msg

  defp non_empty_subset(sub_set_bools, list) do
    sub_set_bools
    |> Enum.zip(list)
    |> Enum.filter(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
  end

  defp is_one?(result, p), do: result > p / 4 and result < 3 * p / 4

  defp abs_normal(), do: abs(rand())

  def calc_m(p, n), do: ((1 + abs_normal()) * (n + 1) * :math.log(p)) |> round

  def generate_keys(p, n, m) do
    pri_k = PrivateKey.generate(p, n)

    pub_k = PublicKey.generate(p, n, m, pri_k)

    {pri_k, pub_k}
  end

  def encrypt_bit(:zero, p, m, pub_k) do
    {ai, bi} = pub_k

    subset = m |> Prob.choose_sub_set()

    sum_ai = subset |> non_empty_subset(ai) |> Zp.sum_vectors(p)
    sum_bi = subset |> non_empty_subset(bi) |> Zp.sum_vector(p)

    {sum_ai, sum_bi}
  end

  def encrypt_bit(:one, p, m, pub_k) do
    {sum_ai, sum_bi} = encrypt_bit(:zero, p, m, pub_k)
    threshold = floor(p / 2)

    {sum_ai, Zp.add(sum_bi, threshold, p)}
  end

  def decrypt_bit(enc_bit, pri_k, p) do
    {sum_ai, sum_bi} = enc_bit
    res = sum_ai |> Zp.internal_prod(pri_k, p) |> (&Zp.subtract(sum_bi, &1, p)).()

    if is_one?(res, p) do
      :one
    else
      :zero
    end
  end

  def encrypt(msg, p, m, pub_k) do
    msg |> Msg.encode() |> Enum.map(&encrypt_bit(&1, p, m, pub_k))
  end

  def decrypt(enc_msg, pri_k, p) do
    enc_msg |> Enum.map(&decrypt_bit(&1, pri_k, p)) |> Msg.decode()
  end
end

eval = fn ->
  # security parameter
  n = 80
  # prime between n^2 and 2n^2
  p = 1973
  # third parameter generated from n and p
  m = Regev.calc_m(p, n)

  msg = "Lula presidente!"

  {pri_k, pub_k} = Regev.generate_keys(p, n, m)

  encrypted = Regev.encrypt(msg, p, m, pub_k)

  decrypted = encrypted |> Regev.decrypt(pri_k, p)

  %{
    encrypted: encrypted,
    decrypted: decrypted
  }
end

eval.()
