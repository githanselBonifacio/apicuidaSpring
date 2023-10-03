package co.com.sura.postgres.repository.agenda.adapter;



import co.com.sura.autoagendador.AutoAgendador;

import co.com.sura.autoagendador.OrigenRegional;
import co.com.sura.autoagendador.Resultado;
import co.com.sura.entity.agenda.*;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.*;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.repository.remision.data.TomaMuestraRepository;
import co.com.sura.postgres.repository.remision.data.CanalizacionRepository;
import co.com.sura.postgres.repository.remision.data.SoporteNutricionalRepository;
import co.com.sura.postgres.repository.remision.data.SondajeRepository;
import co.com.sura.postgres.repository.remision.data.TratamientoRepository;
import co.com.sura.postgres.repository.remision.data.FototerapiaRepository;
import co.com.sura.postgres.repository.remision.data.CuracionRepository;
import co.com.sura.postgres.repository.remision.data.SecrecionRepository;
import co.com.sura.services.mapbox.GeoUbicacion;
import co.com.sura.services.mapbox.MapboxService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static co.com.sura.autoagendador.IdRegional.getByIdCiudad;
import static co.com.sura.constantes.Mensajes.*;
import static co.com.sura.postgres.repository.moviles.data.DesplazamientoData.crearDesplazamientoData;

@Repository
public class AgendaRepositoryAdapter implements AgendaRepository {

    private static final Integer MAXSIZE                       = 2;
    private static final Integer NUMERO_GENERACIONES           = 1000;
    private static final Integer SIZE_POBLACION_INICIAL        = 10;
    private static final Integer NUMERO_PADRES_EMPAREJADOS     = 5;
    private static final Integer HOLGURA_DEFECTO               = 1200;
    private static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;
    private static final String DESPLAZAMIENTO_VISITA          = "dvisita";
    private static final String TIPO_TAREA_VISITA              = "visita";

    private final MapboxService mapboxService;
    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final ProfesionalRepository profesionalRepository;
    private final CitaRepository citaRepository;
    private final DesplazamientoRepository desplazamientoRepository;
    private final TratamientoRepository tratamientoRepository;
    private final CuracionRepository curacionRepository;
    private final CanalizacionRepository canalizacionRepository;
    private final FototerapiaRepository fototerapiaRepository;
    private final SecrecionRepository secrecionRepository;
    private final SondajeRepository sondajeRepository;
    private final SoporteNutricionalRepository soporteNutricionalRepository;
    private final TomaMuestraRepository tomaMuestraRepository;
    private final ConductorRepository conductorRepository;
    private final MovilRepository movilRepository;

    @Autowired
    public AgendaRepositoryAdapter(
            MapboxService mapboxService, TurnoProfesionalesRepository turnoProfesionalesRepository,
            ProfesionalRepository profesionalRepository, CitaRepository citaRepository,
            DesplazamientoRepository desplazamientoRepository, TratamientoRepository tratamientoRepository,
            CuracionRepository curacionRepository, CanalizacionRepository canalizacionRepository,
            FototerapiaRepository fototerapiaRepository, SecrecionRepository secrecionRepository,
            SondajeRepository sondajeRepository, SoporteNutricionalRepository soporteNutricionalRepository,
            TomaMuestraRepository tomaMuestraRepository, ConductorRepository conductorRepository,
            MovilRepository movilRepository) {

        this.mapboxService = mapboxService;
        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.profesionalRepository = profesionalRepository;
        this.citaRepository = citaRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.curacionRepository = curacionRepository;
        this.canalizacionRepository = canalizacionRepository;
        this.fototerapiaRepository = fototerapiaRepository;
        this.secrecionRepository = secrecionRepository;
        this.sondajeRepository = sondajeRepository;
        this.soporteNutricionalRepository = soporteNutricionalRepository;
        this.tomaMuestraRepository = tomaMuestraRepository;
        this.conductorRepository = conductorRepository;
        this.movilRepository = movilRepository;
    }

    @Override
    public Flux<Profesional> consultarProfesionales() {
        return profesionalRepository.findAll()
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalByTurnoCiudad(LocalDate fechaTurno, String idCiudad) {
        return profesionalRepository.findByTurnoRegional(fechaTurno,idCiudad)
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalFromTurnoCiudad(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno) {
        return profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Mono<Boolean> asignarProfesionalTurno(
            LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional) {
        return turnoProfesionalesRepository.save(
                new TurnoProfesionalesData()
                        .toBuilder()
                        .fechaTurno(fechaTurno)
                        .idHorarioTurno(idHorarioTurno)
                        .idProfesional(idProfesional)
                        .build()
        ).then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Mono<Boolean> desasignarProfesionalTurno(
            LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional) {
        return  citaRepository.desagendarAllFromIdProfesional( fechaTurno,  idHorarioTurno, idProfesional)
                .then(Mono.from(turnoProfesionalesRepository.deleteByFechaTurnoIdHorarioProfesional(
                        fechaTurno,idHorarioTurno,idProfesional)))
                .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Flux<Profesional> consultarProfesionalesByIdCiudad(String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterAgenda :: convertToProfesional);
    }

    public Flux<Tarea> consultarTareasTurnoByProfesional(
            ProfesionalData profesionalData,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional){

        return citaRepository
                .findCitasByTurnoCiudadProfesional(fechaTurno, idHorarioTurno, idRegional,
                        profesionalData.getNumeroIdentificacion())
                .map(ConverterAgenda :: convertToTarea)
                .map(tarea -> {
                    tarea.setTipo(TIPO_TAREA_VISITA);
                    return tarea;
                })
                .mergeWith(
                        desplazamientoRepository
                                .findByIdCitaPartidaByProfesional(
                                        fechaTurno,idHorarioTurno,idRegional,profesionalData.getNumeroIdentificacion())
                                .map(ConverterAgenda :: convertToTarea)
                );
    }

    @Override
    public Flux<Actividad> consultarActividadesByProfesionalesCiudadHorarioTurno(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional) {
         return turnoProfesionalesRepository.findTurnoProfesionalByCiudadHorario(
                 fechaTurno,idHorarioTurno,idRegional)
                 .flatMap(profesionalData -> {
                     Flux<Tarea> tareaFlux = consultarTareasTurnoByProfesional(
                             profesionalData, fechaTurno, idHorarioTurno, idRegional
                     );
                     return tareaFlux.collectList().map(citas ->{
                                 List<Tarea> tareaList = new ArrayList<>(citas);
                                 return ConverterAgenda.convertToActividad(profesionalData)
                                         .toBuilder().tareas(tareaList).build();
                             });
                 })
                 .collectList()
                 .flatMapMany(actividades -> Flux.fromIterable(actividades)
                         .sort(Actividad::compareTo));
    }

    @Override
    public Mono<Profesional> crearProfesional(Profesional profesional) {

        return profesionalRepository.existsById(profesional.getNumeroIdentificacion())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(PROFESIONAL_YA_EXISTE.getValue()));
                    }
                    return profesionalRepository.insertProfesional(profesional);
                })
                .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                .map(ConverterAgenda::convertToProfesional);


    }

    @Override
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return Mono.just(profesional)
                .then(profesionalRepository.existsById(profesional.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(PROFESIONAL_NO_EXISTE.getValue()));
                    }
                    return profesionalRepository.save(ConverterAgenda.convertToProfesionalData(profesional));
                })
                .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                .map(ConverterAgenda::convertToProfesional);
    }

    @Override
    public Mono<Conductor> crearConductor(Conductor conductor) {

        return  conductorRepository.existsById(conductor.getNumeroIdentificacion())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(CONDUCTOR_YA_EXISTE.getValue()));
                    }
                    return conductorRepository.insertConductor(conductor);
                })
                .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                .map(ConverterAgenda::converToConductor);
    }

    @Override
    public Mono<Conductor> actualizarConductor(Conductor conductor) {
        System.out.println(conductor.getNumeroIdentificacion());
        return Mono.just(conductor)
                .then(conductorRepository.existsById(conductor.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(CONDUCTOR_NO_EXISTE.getValue()));
                    }
                    return conductorRepository.save(ConverterAgenda.converToConductorData(conductor));
                })
                .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                .map(ConverterAgenda::converToConductor);
    }

    @Override
    public Flux<Conductor> consultarConductores() {
        return conductorRepository.findAll()
                .map(ConverterAgenda::converToConductor);
    }

    @Override
    public Mono<Movil> crearMovil(Movil movil) {
        return null;
    }

    @Override
    public Mono<Movil> actualizarMovil(Movil movil) {
        return null;
    }
    @Override
    public Flux<Movil> consultarMoviles() {
        return movilRepository.findAll()
                .map(ConverterAgenda::convertToMovil);
    }
    @Override
    public Flux<Movil> consultarMovilesSinConductor() {
        return movilRepository.findAllWithoutConductor()
                .map(ConverterAgenda::convertToMovil);
    }
    @Override
    public Flux<Movil> consultarMovilesByIdRegional(String idRegional) {
        return movilRepository.findByIdRegional(idRegional)
                .map(ConverterAgenda::convertToMovil);
    }

    @Override
    public Flux<Cita> consultarCitasByTurnoCiudad(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return citaRepository.findCitasByTurnoCiudad(
               fechaTurno,
                idHorarioTurno,
                idCiudad);
    }

    @Override
    public Mono<Boolean> reprogramarCita(LocalDateTime fechaProgramada,
                                         String idCita,
                                         String idProfesional,
                                         LocalDate fechaTurno,
                                         Integer idHorarioTurno,
                                         String idRegional) {
        return citaRepository.actualizarFechaProgramada(fechaProgramada,idCita)
                .then(calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> agendarToProfesional(
            String idCita, String idProfesional,  LocalDate fechaTurno,Integer idHorarioTurno,String idRegional) {
        return citaRepository.agendarToProfesional(idCita,idProfesional)
                .then(calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> desagendarToProfesional(
            String idCita,String idProfesional,LocalDate fechaTurno,Integer idHorarioTurno,String idRegional) {
        return citaRepository.desagendarToProfesional(idCita)
                .then(calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> desagendarTurnocompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idCiudad)
                .then(citaRepository.desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idCiudad))
                .then(Mono.just(Boolean.TRUE));
    }

    private Mono<Void> asignarListaCitaToProfesionalAutoagendar (
            List<ProfesionalData> profesionalesDataList, Resultado mejoResultado){
        return Flux.fromIterable(profesionalesDataList)
           .flatMap(profesionalData -> {
              int index = profesionalesDataList.indexOf(profesionalData);
              int sizeCitasGen = mejoResultado.getIndividuo().getCitaGen().get(index).size();
              return Flux.fromIterable(mejoResultado.getIndividuo().getCitaGen().get(index).subList(1, sizeCitasGen))
                            .flatMap(citaGenetic -> {
                                String idCita = citaGenetic.getIdCita();
                                String idProfesional = profesionalData.getNumeroIdentificacion();
                                return citaRepository.agendarToProfesional(idCita, idProfesional);
                            });
                }).then();
    }
    private Mono<Void> insertDesplazamientosAllCitasByProfesional(
            LocalDate fechaTurno, String idCiudad,Integer idHorarioTurno){

        return profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
                .collectList()
                .flatMap(profesionalListData -> Flux.fromIterable(profesionalListData)
                        .flatMap(profesionalData -> calcularDesplazamientoCitaByProfesional(
                                fechaTurno,
                                idHorarioTurno,
                                idCiudad,
                                profesionalData.getNumeroIdentificacion()
                        )).then());
    }

    @Override
    public Mono<Boolean> autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
      return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idCiudad)
         .then(citaRepository.findCitasByTurnoCiudad(fechaTurno,idHorarioTurno,idCiudad)
         .collectList()
         .flatMap(citaDataList -> profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
             .collectList().flatMap(profesionalesDataList -> {

               var autoAgendador = new AutoAgendador(
                       OrigenRegional.getOrigenCiudadById(getByIdCiudad(idCiudad)).getCitaGenetic(),
                  ConverterAgenda.convertToListCitaGenetic(citaDataList), profesionalesDataList.size(),
                  NUMERO_GENERACIONES, SIZE_POBLACION_INICIAL, NUMERO_PADRES_EMPAREJADOS, PENALIZACION_HOLGURA_NEGATIVA,
                        mapboxService);

               autoAgendador.run();
               return asignarListaCitaToProfesionalAutoagendar(profesionalesDataList,autoAgendador.mejorSolucion());
             })
         )).then(insertDesplazamientosAllCitasByProfesional(fechaTurno,idCiudad,idHorarioTurno))
              .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad){
        return desplazamientoRepository.findByIdCitaPartida(fechaProgramada,idHorarioTurno,idCiudad)
                .map(ConverterAgenda :: converToDesplazamiento);
    }

    @Override
    public Mono<Boolean> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno,String idRegional, String idProfesional) {

        return desplazamientoRepository.deleteByFechaTurnoProfesional(fechaTurno,idHorarioTurno,idProfesional)
              .then(citaRepository.findCitasByTurnoCiudadProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional)
              .collectList()
              .flatMapMany(Flux::fromIterable)
              .buffer(MAXSIZE,1)
              .filter(citas -> citas.size() == MAXSIZE)
              .flatMap(
                   citas ->{
                      List<DesplazamientoData> desplazamientosList = new ArrayList<>();
                      CitaData citaPartida = citas.get(0);
                      CitaData citaDestino = citas.get(1);

                      var duracionViaje = mapboxService.calcularTiempoViaje(
                              new GeoUbicacion(citaPartida.getLatitud(),citaPartida.getLongitud()),
                              new GeoUbicacion(citaDestino.getLatitud(),citaDestino.getLongitud())).block();

                      var desplazamientoData = crearDesplazamientoData(citaPartida,citaDestino)
                            .toBuilder().tipo(DESPLAZAMIENTO_VISITA)
                            .idHorarioTurno(idHorarioTurno).duracion(duracionViaje).holgura(HOLGURA_DEFECTO).build();
                         desplazamientosList.add(desplazamientoData);

                         return desplazamientoRepository.saveAll(desplazamientosList);
                   }
               )
               .then(Mono.just(Boolean.TRUE))
        );
    }

    @Override
    public Flux<Tratamiento> consultarTratamientoByCitas(String idCita) {
        return tratamientoRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToTratamiento);
    }

    @Override
    public Mono<Procedimientos> consultarProcedimientosByIdCita(String idCita) {
        return Mono.zip(
                        consultarCanalizacionesByCitas(idCita).collectList(),
                        consultarCuracionesByCitas(idCita).collectList(),
                        consultarFototerapiasByCitas(idCita).collectList(),
                        consultarSecrecionesByCitas(idCita).collectList(),
                        consultarSondajesByCitas(idCita).collectList(),
                        consultarTomaMuestrasByCitas(idCita).collectList(),
                        consultarSoporteNutricionalesByCitas(idCita).collectList())
                .map(tuple->Procedimientos.builder()
                                .canalizaciones(tuple.getT1())
                                .curaciones(tuple.getT2())
                                .fototerapias(tuple.getT3())
                                .secreciones(tuple.getT4())
                                .sondajes(tuple.getT5())
                                .tomaMuestras(tuple.getT6())
                                .soporteNutricionales(tuple.getT7())
                                .build()

        );
    }

    @Override
    public Flux<Curacion> consultarCuracionesByCitas(String idCita) {
        return curacionRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToCuracion);
    }

    @Override
    public Flux<Canalizacion> consultarCanalizacionesByCitas(String idCita) {
        return canalizacionRepository.findByIdCita(idCita)
                .map(ConverterAgenda ::convertToCanalizacion);
    }

    @Override
    public Flux<Fototerapia> consultarFototerapiasByCitas(String idCita) {
        return fototerapiaRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToFototerapia);
    }

    @Override
    public Flux<Secrecion> consultarSecrecionesByCitas(String idCita) {
        return secrecionRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToSecrecionData);
    }

    @Override
    public Flux<Sondaje> consultarSondajesByCitas(String idCita) {
        return sondajeRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToSondajeData);
    }

    @Override
    public Flux<TomaMuestra> consultarTomaMuestrasByCitas(String idCita) {
        return tomaMuestraRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToTomaMuestraData);
    }

    @Override
    public Flux<SoporteNutricional> consultarSoporteNutricionalesByCitas(String idCita) {
        return soporteNutricionalRepository.findByIdCita(idCita)
                .map(ConverterAgenda::convertToSoporteNutricionalData);
    }

}
