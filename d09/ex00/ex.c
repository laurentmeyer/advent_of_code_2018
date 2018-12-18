#include <stdlib.h>
#include <stdio.h>
#define PLAYERS 411
#define MAX 7205900

typedef struct          s_list {
    int                 value;
    struct s_list       *next;
    struct s_list       *prev;
}                       t_list ;

t_list          *new_list(int value)
{
    t_list      *res;

    if ((res = (t_list *)malloc(sizeof(t_list))) != NULL)
    {
        res->value = value;
        res->next = res;
        res->prev = res;
    }
    return (res);
}

void            destroy_list(t_list *lst)
{
    t_list  *start = lst;
    t_list  *tmp;

    while (lst != start)
    {
        tmp = lst->next;
        free(lst);
        lst = tmp;
    }
}

void            insert(t_list *at, int value)
{
    t_list      *new = new_list(value);
    t_list      *before = at->prev;

    if (new == NULL)
        exit(0);
    new->next = at;
    new->prev = at->prev;
    at->prev->next = new;
    at->prev = new;
}

int            remove_at(t_list **at)
{
    t_list      *prev = (*at)->prev;
    t_list      *next = (*at)->next;
    int         res = (*at)->value;

    free(*at);
    prev->next = next;
    next->prev = prev;
    *at = next;
    return (res);
}

void        rot(t_list **list, int moves)
{
    t_list      *l = *list;

    if (moves >= 0)
        while (moves-- > 0)
            l = l->next;
    else
        while (moves++ < 0)
            l = l->prev;
    *list = l;
}

void            print_list(t_list *lst)
{
    t_list      *tmp = lst;

    printf("%d", tmp->value);
    tmp = tmp->next;
    while (tmp != lst)
    {
        printf(" %d", tmp->value);
        tmp = tmp->next;
    }
    printf("\n");
}

long long int             max(long long int *scores)
{
    int i = 0;
    long long int max = 0;

    while (i < PLAYERS)
    {
        if (scores[i] > max)
            max = scores[i];
        i++;
    }
    return (max);
}

int main(void)
{
    t_list *board = new_list(0);
    long long int scores[PLAYERS] = {0};
    int stone = 1;
    int player = 1;
    long long int m;

    scores[0] = 1;
    while (stone < MAX)
    {
        if (stone % 23 == 0)
        {
            rot(&board, -7);
            scores[player] += remove_at(&board) + stone;
        }
        else
        {
            rot(&board, 2);
            insert(board, stone);
            rot(&board, -1);
        }
        player = (player + 1) % PLAYERS;
        stone++;
    }
    destroy_list(board);
    printf("max score = %lld\n", max(scores));
    return (0);
}