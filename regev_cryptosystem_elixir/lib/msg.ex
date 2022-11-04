defmodule Msg do
  defp bit_to_symbol(48), do: :zero
  defp bit_to_symbol(49), do: :one

  defp symbol_to_bit(:zero), do: 48
  defp symbol_to_bit(:one), do: 49

  def encode(str) do
    str
    |> :binary.decode_unsigned()
    |> Integer.to_string(2)
    |> to_charlist()
    |> Enum.map(&bit_to_symbol/1)
  end

  def decode(symbols) do
    symbols
    |> Enum.map(&symbol_to_bit/1)
    |> to_string()
    |> Integer.parse(2)
    |> elem(0)
    |> :binary.encode_unsigned()
  end
end
