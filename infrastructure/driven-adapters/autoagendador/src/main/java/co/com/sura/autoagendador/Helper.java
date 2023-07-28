package co.com.sura.autoagendador;

import org.javatuples.Pair;
import reactor.core.publisher.Flux;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

public class Helper {


    public static Individuo intercambiarElementos(
            List<List<CitaGenetic>> citasGen, Pair<Integer,Integer> pos1, Pair<Integer,Integer> pos2) {

            List<List<CitaGenetic>> nuevaListaIndividuo = new ArrayList<>(citasGen);

            var val1 = nuevaListaIndividuo.get(pos1.getValue0()).get(pos1.getValue1());
            var val2 = nuevaListaIndividuo.get(pos2.getValue0()).get(pos2.getValue1());

            List<List<CitaGenetic>> nuevaListaIndividuoCopy = new ArrayList<>(nuevaListaIndividuo);
            nuevaListaIndividuoCopy.get(pos1.getValue0()).set(pos1.getValue1(), val2);
            nuevaListaIndividuoCopy.get(pos2.getValue0()).remove((int)pos2.getValue1());
            nuevaListaIndividuoCopy.get(pos2.getValue0()).add(val1);
            return new Individuo(Flux.fromIterable(nuevaListaIndividuoCopy));

    }

    public static Individuo cambiarElementos(
            List<List<CitaGenetic>> citasGen, Pair<Integer,Integer> pos1, Pair<Integer,Integer> pos2){

        List<List<CitaGenetic>> nuevaListaIndividuo = new ArrayList<>(citasGen);

        var val1 = nuevaListaIndividuo.get(pos1.getValue0()).get(pos1.getValue1());
        List<List<CitaGenetic>> nuevaListaIndividuoCopy = new ArrayList<>(nuevaListaIndividuo);
        nuevaListaIndividuoCopy.get(pos1.getValue0()).remove((int) pos1.getValue1());
        nuevaListaIndividuoCopy.get(pos2.getValue0()).add(val1);
        return new Individuo(Flux.fromIterable(nuevaListaIndividuoCopy));

    }
    public static int maxSumaLista(Flux<List<Integer>> listaFlux) {
        AtomicInteger max = new AtomicInteger();
        listaFlux.flatMap(lista -> Flux.just(lista.stream().mapToInt(Integer::intValue).sum()))
                .reduce(Integer::max)
                .subscribe(max::set);

        return max.get();
    }


    public static List<Integer> encontrarSumandos(int numero, int numerosSumandos) {
        List<Integer> sumandos = new ArrayList<>();
        var suma = 0;
        var random = new Random();

        for (var i = 0; i < numerosSumandos; i++) {
            if (i < numerosSumandos - 1) {
                int limit = numero - numerosSumandos - suma;
                if (limit <= 0) {
                    limit = 1;
                }
                int counter = random.nextInt(limit) + 1;
                sumandos.add(counter);
                suma += counter;
            } else {
                int counter = numero - suma;
                sumandos.add(counter);
                suma += counter;
            }
        }

        return sumandos;
    }
}
