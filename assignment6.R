# 加载所需的库
library(dplyr)
library(tidyr)
library(readr)

# 读取数据文件
scientist <- read_csv("scientist.csv")
inst_wos_dict <- read_csv("inst_wos_dict.csv")
cddt_paper <- read_csv("cddt_paper.csv")
cite <- read_csv("cite.csv")

# 定义函数筛选符合条件的论文
filter_papers <- function(papers, scientist) {
  filtered_papers <- papers %>%
    inner_join(scientist, by = c("inst_wos_dict" = "inst")) %>%
    filter(item_type == 1,
           pub_year >= startyear,
           pub_year <= endyear)
  return(filtered_papers)
}

# 筛选符合条件的论文
filtered_papers <- filter_papers(papers, scientist)

# 通过论文的引用关系获取新论文
while(TRUE) {
  citing_papers <- filtered_papers %>%
    filter(ut_char %in% unique(citing_ut))
  
  # 筛选新引用论文
  new_papers <- papers %>%
    inner_join(citing_papers, by = c("ut_char" = "citing_ut")) %>%
    filter(!ut_char %in% filtered_papers$ut_char)
  
  # 如果没有新引用论文，退出循环
  if(nrow(new_papers) == 0) {
    break
  }
  
  # 将新引用论文加入已筛选论文中
  filtered_papers <- bind_rows(filtered_papers, new_papers)
}

# 筛选新添加论文，使其满足发表地址、论文发表时间与科学家履历匹配
final_papers <- filter_papers(filtered_papers, scientist)

# 输出最终筛选的论文清单
write.csv(final_papers, "final_papers.csv", row.names = FALSE)