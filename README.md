# prepar

![](https://img.shields.io/github/last-commit/lmn-labest/prepar?style=plasti&ccolor=blue)
![](https://img.shields.io/badge/Autor-Henrique%20C%20C%20de%20Andrade-blue)
[![Prepar build](https://github.com/lmn-labest/prepar/actions/workflows/build.yml/badge.svg)](https://github.com/lmn-labest/prepar/actions/workflows/build.yml)

Index
- [prepar](#prepar)
  - [1) Compilar o metis no linux](#1-compilar-o-metis-no-linux)
  - [2) Compilar o prepar](#2-compilar-o-prepar)
  - [3) Rodando o prepar](#3-rodando-o-prepar)
  - [4) Exemplo](#4-exemplo)
  - [5) Pre-compilados](#5-pre-compilados)
  - [6) Docker](#6-docker)
  - [7) Usando no WSL](#7-usando-no-wsl)

## 1) Compilar o metis no linux

Para compilar o metis basta seguir os seguintes passos:

```console
cd metisLib
tar -xvzf metis-5.1.0.tar.gz
cd metis-5.1.0
make config
make
mkdir -p ./../../lib/
cp -v build/Linux-x86_64/libmetis/libmetis.a ../../lib/
cd ../../
```

Estes comandos irão descompactar e compilar o `metis5`, configurar o `make`, excetar a compilação e copiar automaticamente a `libmetis.a` para a pasta `lib`. A `libmetis.a` será utilizada depois para a compilação do `prepar`

> ⚠️⚠️ OBS: Você precisa ter instalado o `cmake` por causa do metis.

> ⚠️⚠️ OBS: As versões novas do metis que estão  disponiveis no `github` precisam dessa lib extra `libGKlib`. As versões mais antigas do `metis` podem ser encontrados [aqui](http://glaros.dtc.umn.edu/gkhome/metis/metis/download). As `prepar` só funciona com as versão antigas do `metis`.


## 2) Compilar o prepar

O primeiro passo é fazer uma copia do `Makefile_base`

```console
cp contrib/Makefile_base Makefile
```

Antes de compilar o `prepar` a estrutura do projeto tem que ser algo similar à:

```console
├── include
│   ├── elementos.fi
│   ├── gauss.fi
│   ├── load.fi
│   ├── parallel.fi
│   ├── readfile.fi
│   ├── string.fi
│   ├── termprop.fi
│   ├── time.fi
│   └── transiente.fi
├── lib
│   └── libmetis.a
├── Makefile
└── src
    ├── Adjacency.f
    ├── Filenames.f
    ├── Main.f
    ├── Malloc.f
    ├── Mpi_comunicate.f
    ├── Parallel_imp.f
    ├── Partdmesh.f
    ├── Pdmetis.f
    ├── Propnode.f
    ├── Rdata.f
    ├── Read_mef.f
    ├── Read_metis.f
    ├── Time.f
    ├── Vtk.f
    ├── Write_mef.f
    ├── Write_par.f
    └── Write_vtk.f
```

Agora para compilar fazer basta:

```console
make
```

O executável do `prepar` estará na pasta `bin`. Como foi usando a opção `-static` pode ser que seja necessario instalar lib extras no sistema como a `glibc-static`. Outra opção é tirar a opção `-static` do `Makefile`.

## 3) Rodando o prepar

Criar um arquivo `pre.dat` com o conteudo

```
input  mesh.dat
output     part
div          12
method non-overllaping
partVtk     yes
partMeshVtk yes
partMeshMef yes
meshLoads    no
vtkBin      yes
memory     1000
end
```

## 4) Exemplo

Exsite um exemplo de aquivo de entrada na pastas  `contrib/bin/`. Para usa-lo primeiro vamos descompacta-lo para a pasta `bin/`.

```console
mkdir -p bin
tar -xvzf contrib/examples/solo.tar.gz -C bin/
```

Agora para gerar o particionamento basta executar o `prepar` na pasta `bin/`.

```console
./prepar solo/pre.dat
```

Após rodar o `prepar` a pasta dever ser 

```console
solo/
├── config
│   ├── grav.dat
│   ├── poromec.config
│   ├── poromec.solver
│   └── setprint.dat
├── mesh
│   ├── solo1_boun.dat
│   ├── solo1_coor.dat
│   ├── solo1_elmt.dat
│   ├── solo1_elmtloads.dat
│   └── solo1_initial.dat
├── output
├── part
│   ├── solo1_par_0.dat
│   ├── solo1_par_1.dat
│   ├── solo1_par_2.dat
│   ├── solo1_par_3.dat
│   ├── solo1_par_4.dat
│   ├── solo1_par_5.dat
│   ├── solo1_par_n_0_my_part.vtk
│   ├── solo1_par_n_1_my_part.vtk
│   ├── solo1_par_n_2_my_part.vtk
│   ├── solo1_par_n_3_my_part.vtk
│   ├── solo1_par_n_4_my_part.vtk
│   ├── solo1_par_n_5_my_part.vtk
│   ├── solo1_par_n_6_part.vtk
│   └── solo1_par_pre_t_6.txt
├── pre.dat
├── run
│   ├── nloads.dat
│   ├── node.dat
│   └── run.dat
└── solo1.dat
```

As pastas `run`, `mesh`, `config` e o arquivo `solo1.dat` são os arquivos de entrada da simulação do `mefpar` em sequencial. Na pasta `solo/part` ficam salvos os arquivos do particionamento `*.dat` e `*.vtk`. Os arquivos `solo1_par_*.dat` são arquivos de entrada para a simulação em paralelo do `mefpar`.

Exemplo da malha particonada em 6 partições

![](doc/part_mesh.png)

> ⚠️⚠️ OBS: A estrura de pastas dos arquivos de `input` e `output` podem ser mudadas. Essa estrutura é apenas a que eu gosto, mas você pode ficar a vontade de experimentar outras.


## 5) Pre-compilados

Versões pré compilados para `linux` pode ser encotradas aqui [binarios](https://github.com/lmn-labest/prepar/releases/tag/0.1.0)

## 6) Docker

Caso você queria rodar utilizando containers temos um `Dockerfile` configurado.

Para gerar a imagem, este procedimento precisa ser feito apenas uma única vez:

```console
docker build -t prepar:latest .
```

Para executar o `prepar` e gerar o parcionamento:

```console
docker run --rm -it -v "$(pwd)/bin/solo/:/usr/build/solo/" prepar solo/pre.dat
```

> ⚠️⚠️ OBS: O docker pode ser usado no windows também, mas irá usar bastante recurso da máquina.

## 7) Usando no WSL

Você precisará instalar o `make`, `cmake`, `g++`. Usando o `Ubuntu 22.04.06 LTS` basta utilizar os comandos:

```
sudo apt update
sudo apt install make cmake g++ gfortran
```

Após isso você pode seguir o procedimento normal de instalação no linux.


