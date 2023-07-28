package co.com.sura.autoagendador;

import co.com.sura.services.mapbox.GeoUbicacion;
import co.com.sura.services.mapbox.MapboxService;
import lombok.Data;
import org.javatuples.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static co.com.sura.autoagendador.Helper.*;

@Data
public class AutoAgendador {
    private static final Integer DOS = 2;
    private static final Double UMBRAL = 0.5;

    private MapboxService mapboxService;
    private Helper helper;
    private CitaGenetic origen;
    private List<CitaGenetic> citas;
    private Integer numeroMoviles;
    private Integer numeroGeneraciones;
    private Integer sizePoblacionInicial;
    private Integer numeroPadresEnparejados;
    private double penalizacionHolgura;
    private Runnable calcularDesplazamiento;

    private Map<String,Resultado> resultadoActual;
    private Flux<Desplazamiento> desplazamientos;
    private Poblacion poblacionActual;
    private Double aptitudGlobalActual;

    private final Random random;

    @Autowired
    public AutoAgendador(
            CitaGenetic origen,
            List<CitaGenetic> citas,
            Integer numeroMoviles,
            Integer numeroGeneraciones,
            Integer sizePoblacionInicial,
            Integer numeroPadresEnparejados,
            Double penalizacionHolgura,
            MapboxService mapboxService) {

        this.origen = origen;
        this.citas = new ArrayList<>(citas);
        this.numeroMoviles = numeroMoviles;
        this.numeroGeneraciones = numeroGeneraciones;
        this.sizePoblacionInicial = sizePoblacionInicial;
        this.numeroPadresEnparejados = numeroPadresEnparejados;
        this.penalizacionHolgura = penalizacionHolgura;

        this.desplazamientos = Flux.empty();
        this.resultadoActual = new HashMap<>();

        this.random = new Random();
        this.mapboxService =mapboxService;
    }


    public void crearPoblacionInicial(){
       var poblacionInicial = new Poblacion();
        var individuos = Flux.range(0, this.sizePoblacionInicial)
                .map(i -> Individuo.builder()
                        .citaGen(Flux.fromIterable(
                                CitaGenetic.dividirListaCitas(this.citas,this.numeroMoviles,this.origen)))
                        .build());

        poblacionInicial.setIndividuos(individuos);
        this.poblacionActual = poblacionInicial;
    }
    private Integer validarDuracionViajeAlmacenado(String idCita1, String idCita2){
        AtomicReference<Integer> duracion = new AtomicReference<>(0);
         this.desplazamientos
                .filter(desplazamiento -> desplazamiento.getPos1().equals(idCita1)
                        && desplazamiento.getPos2().equals(idCita2))
                .map(Desplazamiento::getTiempo)
                .next()
                 .subscribe(duracion::set);
        return duracion.get();
    }
    private Integer getDuracionDesplazamiento(CitaGenetic citaPartida, CitaGenetic citaDestino){
        Integer tiempoDesplazamiento;
        Integer consultaTiempoDesplazamiento = validarDuracionViajeAlmacenado(
                citaPartida.getIdCita(),
                citaDestino.getIdCita()
        );
        if (consultaTiempoDesplazamiento == null) {
            tiempoDesplazamiento = calcularTiempoDesplazamiento(
                    citaPartida, citaDestino);

            this.desplazamientos.concatWith(
                    Flux.just(Desplazamiento.crearDesplazamiento(
                            citaPartida, citaDestino, tiempoDesplazamiento)
                    )
            );
        } else {
            tiempoDesplazamiento = consultaTiempoDesplazamiento;
        }
        return tiempoDesplazamiento;
    }

    private   Integer calcularTiempoDesplazamiento(CitaGenetic citaPartida,CitaGenetic citaDestino){
        AtomicReference<Integer> tiempo = new AtomicReference<>();
        var pos1 = GeoUbicacion
                .builder()
                .latitud(citaPartida.getLatitud())
                .longitud(citaPartida.getLongitud())
                .build();

        var pos2 =GeoUbicacion
                .builder()
                .latitud(citaDestino.getLatitud())
                .longitud(citaDestino.getLongitud())
                .build();

        mapboxService.calcularTiempoViaje(pos1,pos2)
                .subscribe(tiempo::set);
        return tiempo.get();
    }
    private Flux<List<Integer>> calcularAptitudIndividuo(Individuo individuo){
      return individuo.getCitaGen()
        .flatMap(c -> {
          List<Integer> holgurasAcumuladas = new ArrayList<>();
          for (var i = 0; i < c.size() - 1; i++) {
            Integer tiempoViaje = getDuracionDesplazamiento(c.get(i),c.get(i + 1)) + c.get(i).getDuracion();
            int holguraAcumulada = CitaGenetic.calcularHolguraAcomulada (c.get(i),c.get(i + 1),tiempoViaje);
            if(holguraAcumulada < 0 ){
                holguraAcumulada = (int)(-1 * Math.abs(holguraAcumulada * (this.penalizacionHolgura)));
            }
            holgurasAcumuladas.add(holguraAcumulada);
          }
            return Flux.just(holgurasAcumuladas);
        });
    }
    private static Double calcularAptitudGlobalIndividuo(Flux<List<Integer>> aptitudIndiviuo){
        AtomicReference<Double> puntaje = new AtomicReference<>();
        aptitudIndiviuo
                .flatMapIterable(list -> list)
                .reduce(0, Integer::sum)
                .map(Double::valueOf)
                .subscribe(puntaje::set);
        return puntaje.get();
    }
    private  Poblacion seleccionarMejoresPadres(){
       return new Poblacion(
               Flux.fromIterable(this.resultadoActual.values())
                       .take(this.numeroPadresEnparejados)
                       .map(Resultado::getIndividuo)
       );
    }
    private void calcularAptitudPoblacion(){
      this.poblacionActual.getIndividuos().subscribe(
        resp ->{
            this.poblacionActual.getIndividuos()
                    .flatMap(individuo -> {
                        var puntajeAptitudIndividuo = this.calcularAptitudIndividuo(individuo);
                        var puntajeGlobalAptitudIndividuo = calcularAptitudGlobalIndividuo(puntajeAptitudIndividuo);
                        return Mono.just(new Resultado()
                                .toBuilder()
                                .puntajeAptitudIndividuo(puntajeAptitudIndividuo)
                                .individuo(individuo)
                                .puntajeAptitudGlobalIndividuo(puntajeGlobalAptitudIndividuo)
                                .build());
                    })
                    .collectList()
                    .map(resultados -> resultados.stream()
                            .sorted(Comparator.comparingDouble(Resultado::getPuntajeAptitudGlobalIndividuo).reversed())
                            .limit(this.numeroPadresEnparejados)
                            .collect(Collectors.toMap(
                                    resultado -> UUID.randomUUID().toString(),
                                    resultado -> resultado,
                                    (oldValue, newValue) -> oldValue,
                                    LinkedHashMap::new
                            ))
                    ).subscribe(result -> this.resultadoActual = result);
        }
      );


    }

    private static  Flux<Pair<Integer,Integer>> encontrarHorguraNegativa(Flux<List<Integer>> puntajeIndividuo, int ic){
        return puntajeIndividuo
                .flatMapSequential(list -> Flux.fromIterable(list)
                        .index()
                        .filter(tuple -> tuple.getT2() < 0)
                        .map(tuple -> {
                            var i = puntajeIndividuo.index().getPrefetch();
                            var j = tuple.getT1().intValue();
                            int c = (j + ic < list.size()) ? (j + ic) : j;
                            return new Pair<>(i, c);
                        })
                )
                .filter(pair -> pair.getValue1() != 0);
    }

    private Poblacion mutarIndividuosPoblacion() {

        List<Individuo> nuevosIndividuos = new ArrayList<>();

        for (Resultado resultado: this.resultadoActual.values()) {

            var coor =
                    encontrarHorguraNegativa(
                            resultado.getPuntajeAptitudIndividuo(), random.nextInt(DOS));

            AtomicReference<List<Pair<Integer,Integer>>>coorListAtomic = new AtomicReference<>();

           coor.collectList().subscribe(coorListAtomic::set);
            var coorList = coorListAtomic.get();
            if (coorList.isEmpty()){
                return new Poblacion();
            }

            var pos1 = coorList.get(random.nextInt(coorList.size()));
            var pos2 = new Pair<>(maxSumaLista(resultado.getPuntajeAptitudIndividuo()),1);
            Individuo nuevoIndividuo;

            AtomicReference<List<List<CitaGenetic>>> citasGenetic = new AtomicReference<>();
            resultado.getIndividuo().clone()
                    .getCitaGen()
                    .collectList()
                    .subscribe(citasGenetic::set);
            if (random.nextDouble() > UMBRAL) {
                nuevoIndividuo = intercambiarElementos(citasGenetic.get(), pos1, pos2);
            } else {
                nuevoIndividuo = cambiarElementos(citasGenetic.get(), pos1, pos2);
            }

            nuevosIndividuos.add(nuevoIndividuo);
        }
        nuevosIndividuos = nuevosIndividuos
                .stream()
                .peek(Individuo::sortCitaGen)
                .collect(Collectors.toList());

        return new Poblacion(Flux.fromIterable(nuevosIndividuos));
    }

    public Resultado mejorSolucion() {

        var mejorResultado = Collections.max(
                this.resultadoActual
                        .values(),Comparator.comparing(Resultado::getPuntajeAptitudGlobalIndividuo)
        );
        List<Pair<Integer,Integer>> coordenadasHolguraNegativa = encontrarHorguraNegativa(
                mejorResultado.getPuntajeAptitudIndividuo(),1
        ).collectList().blockOptional().orElse(new ArrayList<>());
        var individuo = mejorResultado.getIndividuo();

        List<List<CitaGenetic>> nuevalistaCitasGen = new ArrayList<>();
        var listaCitas = individuo.getCitaGen().collectList().blockOptional()
                .orElse(new ArrayList<>());
        for(var i = 0;i<listaCitas.size();i++){
            List<CitaGenetic> itemIndividuo = new ArrayList<>();
            for(var  j = 0; j<listaCitas.get(i).size();j++){
                if(!coordenadasHolguraNegativa.contains(new Pair<>(i,j))){
                    itemIndividuo.add(listaCitas.get(i).get(j));
                }

            }
            nuevalistaCitasGen.add(itemIndividuo);
        }

        mejorResultado.setIndividuo( new Individuo(Flux.fromIterable(nuevalistaCitasGen)));
        return mejorResultado;
    }
    public void run (){
        this.crearPoblacionInicial();
        System.out.println(this.resultadoActual);
       /* this.poblacionActual.getIndividuos().subscribe(
                v -> v.getCitaGen().subscribe(r -> System.out.println(r))
        );*/
        for(var g = 0;g<this.numeroGeneraciones;g++){
            this.calcularAptitudPoblacion();
            this.poblacionActual = this.seleccionarMejoresPadres();
            var siguienteGeneracion = this.mutarIndividuosPoblacion();
            final var sizeSiguienteGeneracion = new AtomicInteger();
            siguienteGeneracion
                    .getIndividuos()
                    .collectList()
                    .subscribe(result ->{
                        sizeSiguienteGeneracion.set(result.size());
                    });
            if(sizeSiguienteGeneracion.get() == 0){
                break;
            }else{
                this.poblacionActual = siguienteGeneracion.clone();
            }
        }
    }
}
