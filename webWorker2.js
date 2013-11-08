var numbers = [];

postMessage("waiting");

for(i = 0; i < 10000; i++){
	numbers.push(Math.random() * 100);
}

for(i = 0; i < numbers.length - 1; i++){
	for(j = 0; j < numbers.length - 1; j++){
		if(numbers[j] > numbers[j + 1])
		{
			z = numbers[j];
			numbers[j] = numbers[j + 1];
			numbers[j + 1] = z;
		}
	}
}

postMessage("done");
