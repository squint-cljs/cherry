# Advent of Code solutions

[2023 day 3](https://squint-cljs.github.io/cherry/?boilerplate=https%3A%2F%2Fgist.githubusercontent.com%2Fborkdude%2Fcf94b492d948f7f418aa81ba54f428ff%2Fraw%2F3e871513ab6f2462841f75fc99668f841f33dabb%2Faoc_ui.cljs&repl=true&src=OzsgSGVscGVyIGZ1bmN0aW9uczoKOzsgKGZldGNoLWlucHV0IHllYXIgZGF5KSAtIGdldCBBT0MgaW5wdXQKOzsgKGFwcGVuZCBzdHIpIC0gYXBwZW5kIHN0ciB0byBET00KOzsgKHNweSB4KSAtIGxvZyB4IHRvIGNvbnNvbGUgYW5kIHJldHVybiB4CgooZGVmIGlucHV0ICgtPj4gKGpzLWF3YWl0IChmZXRjaC1pbnB1dCAyMDIzIDMpKQogICAgICAgICAgICAgc3RyL3NwbGl0LWxpbmVzKSkKCihkZWZuIGRpZ2l0PyBbeF0KICAoI3tcMSBcMiBcMyBcNCBcNSBcNiBcNyBcOCBcOSBcMH0geCkpCgooZGVmbiAtPmdyaWQKICAoW3Jhd10gKC0%2BZ3JpZCBuaWwgcmF3KSkKICAoW3ByZWQgcmF3XQogICAoLT4%2BIHJhdwogICAgICAgIChtYXAtaW5kZXhlZAogICAgICAgICAgKGZuIFt5IHJvd10KICAgICAgICAgICAgKC0%2BPiByb3cgKG1hcC1pbmRleGVkCiAgICAgICAgICAgICAgICAgICAgICAgKGZuIFt4IGNoYXJdCiAgICAgICAgICAgICAgICAgICAgICAgICAoaWYgcHJlZAogICAgICAgICAgICAgICAgICAgICAgICAgICAod2hlbiAocHJlZCBjaGFyKQogICAgICAgICAgICAgICAgICAgICAgICAgICAgIFtbeCB5XSBjaGFyXSkKICAgICAgICAgICAgICAgICAgICAgICAgICAgW1t4IHldIGNoYXJdKSkpCiAgICAgICAgICAgICAgICAgKGZpbHRlciBzZXEpCiAgICAgICAgICAgICAgICAgKGludG8gW10pKSkpCiAgICAgICAgKGZpbHRlciBzZXEpCiAgICAgICAgKGludG8gW10pCiAgICAgICAgKGFwcGx5IGNvbmNhdCkKICAgICAgICAoaW50byB7fSkpKSkKCihkZWZuIHBhcnQ%2FIFtjaGFyXQogIChub3QgKG9yICg9IFwuIGNoYXIpIChkaWdpdD8gY2hhcikpKSkKCihkZWZuIC0%2BcGFydC1sb2NzIFtyYXddCiAgKC0%2BZ3JpZCBwYXJ0PyByYXcpKQoKKGNvbW1lbnQKICAoLT5wYXJ0LWxvY3MgaW5wdXQpCiAgKC0%2BZ3JpZCBpbnB1dCkKCiAgKC0%2BPiAiNDUuLiouNCIgKG1hcCAoZm4gW2NoYXJdIChkaWdpdD8gY2hhcikpKSkpCgooZGVmbiBuZWlnaGJvcnMgW1t4IHldXQogICN7W3ggKGluYyB5KV0KICAgIFt4IChkZWMgeSldCiAgICBbKGluYyB4KSB5XQogICAgWyhkZWMgeCkgeV0KICAgIFsoaW5jIHgpIChpbmMgeSldCiAgICBbKGRlYyB4KSAoZGVjIHkpXQogICAgWyhkZWMgeCkgKGluYyB5KV0KICAgIFsoaW5jIHgpIChkZWMgeSldfSkKCihkZWZuIHgtbnVtLW5laWdoYm9ycyBbZ3JpZCBbeCB5XV0KICAobGV0IFstPngtbmJycwogICAgICAgIChmbiBbLT54XQogICAgICAgICAgKGxvb3AgW25ldy1jb29yZCBbKC0%2BeCB4KSB5XSBuYnJzIFtdXQogICAgICAgICAgICAoY29uZAogICAgICAgICAgICAgIChuaWw%2FIChncmlkIG5ldy1jb29yZCkpIG5icnMKICAgICAgICAgICAgICAoZGlnaXQ%2FIChncmlkIG5ldy1jb29yZCkpCiAgICAgICAgICAgICAgKHJlY3VyIFsoLT4gKGZpcnN0IG5ldy1jb29yZCkgLT54KSB5XSAoY29uY2F0IG5icnMgW25ldy1jb29yZF0pKQogICAgICAgICAgICAgIDplbHNlICAgICAgICAgICAgICAgICAgIG5icnMpKSkKICAgICAgICBsZWZ0LW5icnMgICgtPngtbmJycyBkZWMpCiAgICAgICAgcmlnaHQtbmJycyAoLT54LW5icnMgaW5jKV0KICAgIChjb25jYXQgKHJldmVyc2UgbGVmdC1uYnJzKSBbW3ggeV1dIHJpZ2h0LW5icnMpKSkKCihjb21tZW50CiAgKHgtbnVtLW5laWdoYm9ycyAoLT5ncmlkIGlucHV0KSBbMyAyXSkKICAoeC1udW0tbmVpZ2hib3JzICgtPmdyaWQgaW5wdXQpIFsyIDJdKQogICh4LW51bS1uZWlnaGJvcnMgKC0%2BZ3JpZCBpbnB1dCkgWzAgMF0pCiAgKQoKKGRlZm4gY29vcmQtPm51bWJlciBbZ3JpZCBjb29yZF0KICAobGV0IFtudW0tY29vcmRzICh4LW51bS1uZWlnaGJvcnMgZ3JpZCBjb29yZCldCiAgICBbKGZpcnN0IG51bS1jb29yZHMpCiAgICAgKC0%2BPiBudW0tY29vcmRzCiAgICAgICAgICAobWFwIGdyaWQpCiAgICAgICAgICAoYXBwbHkgc3RyKQogICAgICAgICAgKHBhcnNlLWxvbmcpKV0pKQoKKGNvbW1lbnQKICAoY29vcmQtPm51bWJlciAoLT5ncmlkIGlucHV0KSBbMiAyXSkKICAoY29vcmQtPm51bWJlciAoLT5ncmlkIGlucHV0KSBbMCAwXSkKICApCgoKKGRlZm4gcGFydC1udW1iZXJzCiAgKFtyYXddIChwYXJ0LW51bWJlcnMgbmlsIHJhdykpCiAgKFtvcHRzIHJhd10KICAgKGxldCBbZ3JpZCAgICAgICgtPmdyaWQgcmF3KQogICAgICAgICBwYXJ0LWxvY3MgKC0%2BcGFydC1sb2NzIHJhdyldCiAgICAgOzsgc3ltYm9scy0%2BbmVpZ2hib3ItZGlnaXRzCiAgICAgKGNvbmQtPj4gcGFydC1sb2NzCiAgICAgICB0cnVlCiAgICAgICAobWFwIChmbiBbW2Nvb3JkIHN5bV1dCiAgICAgICAgICAgICAgW3N5bQogICAgICAgICAgICAgICAobGV0IFtuYnJzIChuZWlnaGJvcnMgY29vcmQpXQogICAgICAgICAgICAgICAgICgtPj4gbmJycwogICAgICAgICAgICAgICAgICAgICAgKGZpbHRlciAoZm4gW25icl0KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAobGV0IFtjaGFyIChncmlkIG5icildCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoZGlnaXQ%2FIGNoYXIpKSkpCiAgICAgICAgICAgICAgICAgICAgICAobWFwIChmbiBbbmJyXSBbbmJyIChncmlkIG5icildKSkpKV0pKQoKICAgICAgICg6c3ltYm9sIG9wdHMpIChmaWx0ZXIgKGZuIFtbc3ltIF9uYnJzXV0KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoI3soOnN5bWJvbCBvcHRzKX0gc3ltKSkpCgogICAgICAgdHJ1ZQogICAgICAgKG1hcCAoZm4gW1tfc3ltIG5icnNdXQogICAgICAgICAgICAgICgtPj4gbmJycwogICAgICAgICAgICAgICAgICAgKG1hcCAoZm4gW1tjb29yZCBfZGlnaXRdXQogICAgICAgICAgICAgICAgICAgICAgICAgIChjb29yZC0%2BbnVtYmVyIGdyaWQgY29vcmQpKSkKICAgICAgICAgICAgICAgICAgIChpbnRvIHt9KSkpKQoKICAgICAgICg6cGFydC1jb3VudCBvcHRzKQogICAgICAgKGZpbHRlciAoZm4gW3hzXQogICAgICAgICAgICAgICAgICg9ICg6cGFydC1jb3VudCBvcHRzKSAoY291bnQgeHMpKSkpKSkpKQoKKGNvbW1lbnQKICA7OyBwYXJ0IDEKICAoLT4%2BCiAgICAocGFydC1udW1iZXJzIGlucHV0KQogICAgKG1hcCB2YWxzKQogICAgKGFwcGx5IGNvbmNhdCkKICAgIChyZWR1Y2UgKykpCgogIDs7IHBhcnQgMgogICgtPj4KICAgIGlucHV0CiAgICAocGFydC1udW1iZXJzCiAgICAgIHs6c3ltYm9sICAgICBcKgogICAgICAgOnBhcnQtY291bnQgMn0pCiAgICAobWFwIHZhbHMpCiAgICAobWFwICMoYXBwbHkgKiAlKSkKICAgIChyZWR1Y2UgKykpICAKICAp), ported from [here](https://github.com/russmatney/advent-of-code/blob/master/src/_2023/_03/core.clj)