# prepar

Index
- [prepar](#prepar)
- [Compilar o metis](#compilar-o-metis)
- [Compilar o pre](#compilar-o-pre)
- [Rodando o pre](#rodando-o-pre)
- [Exemplo](#exemplo)

# Compilar o metis

Descompactar e compilar o metis5

```console
cd metisLib
tar -xvzf metis-5.1.0.tar.gz
cd metis-5.1.0
make config
make
cp -v metis-5.1.0/build/lib/lib/libmetis.a ../lib
cd ../../
```

```console
lib
└── libmetis.a
```

As versões novas do metis precisam dessa lib extra `libGKlib`. As versões mais antigas do metis podem ser encontrados [aqui](http://glaros.dtc.umn.edu/gkhome/metis/metis/download).


# Compilar o pre

Fazer uma copia do `Makefile_base`

```console
cp contrib/Makefile_base Makefile
```

Para compilar basta:

```console
make
```

# Rodando o pre

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

Para rodar basta

```
prepar pre.dar
```

# Exemplo


Descompactando o exemplo que esta na pasta `contrib/bin/` para `bin/`.

```console
tar -xvzf contrib/examples/solo.tar.gz -C bin/
```

Gerando o particionamento executando o `prepar` na pasta `bin/solo/`

```console
prepar pre.dat
```
