#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define NONE '.'

int main()
{
	FILE				*fp;
    char    			*str = NULL;
	unsigned long		count = 0;
	size_t				len = 0;
    int     			i = 0;
    int     			j = 1;
	char				c;

	fp = fopen("./input.txt", "r");
	getline(&str, &len, fp);
	len = strlen(str);
    while ((c = str[j]) != '\0' && c != '\n')
    {
		while (i >=0 && str[i] == NONE)
			i--;
		if (i < 0 || !(str[i] == c + 'A' - 'a' || str[i] + 'A' - 'a' == c))
			i = j++;
		else
        {
			count += 2;
       		str[i] = NONE;
        	str[j++] = NONE;
        }
    }
    printf("%lu\n", len - count);
	fclose(fp);
	free(str);
    return (0);
}