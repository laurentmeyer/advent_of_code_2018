#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define NONE '.'

int main()
{
	FILE				*fp;
    char    			*str = NULL;
    char    			*cpy = NULL;
	unsigned long		count = 0;
	size_t				len = 0;
    int     			i = 0;
    int     			j = 1;
	char				c;
	char				d = 'a';
	int					min = INT32_MAX;

	fp = fopen("./input.txt", "r");
	getline(&str, &len, fp);
	len = strlen(str);
	cpy = (char *)malloc(len + 1);
	while (d <= 'z')
	{
		i = 0;
		j = 0;
		while (str[j] != '\0')
		{
			if (str[j] == d || str[j] == d + 'A' - 'a')
				j++;
			else
				cpy[i++] = str[j++];
		}
		cpy[i] = '\0';
		i = 0;
		j = 1;
		count = strlen(cpy);
		while ((c = cpy[j]) != '\0' && c != '\n')
		{
			while (i >= 0 && cpy[i] == NONE)
				i--;
			if (i < 0 || !(cpy[i] == c + 'A' - 'a' || cpy[i] + 'A' - 'a' == c))
				i = j++;
			else
			{
				count -= 2;
				cpy[i] = NONE;
				cpy[j++] = NONE;
			}
		}
		if (count < min)
			min = count;
		d++;
	}
	printf("%d\n", min);
	fclose(fp);
	free(str);
	free(cpy);
	return (0);
}