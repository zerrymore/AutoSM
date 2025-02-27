#!/bin/bash

# 创建results文件夹，如果没有的话
mkdir -p results

# 初始化计数器
counter=1

# 遍历当前目录下所有以-P.spthy结尾的文件
for file in *-P.spthy; do
    # 提取文件名的前缀（去掉-P.spthy部分）
    base_name="${file%-P.spthy}"
    
    # 执行tamarin-prover命令并将结果存储到results文件夹中
    tamarin-prover "$file" -m=msr > "results/${base_name}_proof.spthy"
    
    # 复制原始-P.spthy文件到results文件夹
    cp "$file" "results/$file"
    
    # 输出处理完成的信息，带有序号
    echo "处理完成 [$counter]: $file -> results/$base_name.proof, 同时复制到 results/$file"
    
    # 增加计数器
    ((counter++))
done
