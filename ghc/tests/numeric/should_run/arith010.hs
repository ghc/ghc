--	Tests enumerations

main = do
	print [1..10]
	print [10..1]		-- []
	print [1,3..10]
	print [10,8..1]
	print ['a'..'f']
	print ['f'..'a']	-- []
	print ['a','c'..'m']
	print ['m','l'..'a']
