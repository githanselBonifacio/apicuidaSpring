package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.autoagendar.AutoAgendador;
import co.com.sura.autoagendar.CitaGenetic;
import co.com.sura.autoagendar.Resultado;
import co.com.sura.entity.agenda.*;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.*;
import co.com.sura.postgres.repository.remision.data.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;


import static co.com.sura.autoagendar.IdCiudad.getByIdCiudad;
import static co.com.sura.autoagendar.OrigenCiudad.getOrigenCiudadById;
import static co.com.sura.postgres.repository.agenda.data.DesplazamientoData.crearDesplazamientoData;

@Repository
public class AgendaRepositoryAdapter implements AgendaRepository {

    private static final Integer MAXSIZE                   = 2;
    private static final Integer NUMERO_GENERACIONES       = 2000;
    private static final Integer SIZE_POBLACION_INICIAL    = 10;
    private static final Integer NUMERO_PADRES_EMPAREJADOS = 5;
    private static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;

    private static final String DESPLAZAMIENTO_VISITA = "dvisita";
    private static final String TIPO_TAREA_VISITA = "visita";
    @Autowired
    private ProfesionalRepository profesionalRepository;

    @Autowired
    private TurnoProfesionalesRepository turnoProfesionalesRepository;

    @Autowired
    private CitaRepository citaRepository;

    @Autowired
    private DesplazamientoRepository desplazamientoRepository;
    @Autowired
    private TratamientoRepository tratamientoRepository;

    @Autowired
    private CuracionRepository curacionRepository;

    @Autowired
    private CanalizacionRepository canalizacionRepository;

    @Autowired
    private FototerapiaRepository fototerapiaRepository;

    @Autowired
    private SecrecionRepository secrecionRepository;

    @Autowired
    private SondajeRepository sondajeRepository;

    @Autowired
    private SoporteNutricionalRepository soporteNutricionalRepository;

    @Autowired
    private TomaMuestraRepository tomaMuestraRepository;

    @Override
    public Flux<Profesional> consultarProfesionales() {
        return profesionalRepository.findAll()
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalByTurnoCiudad(LocalDate fechaTurno, String idCiudad) {
        return profesionalRepository.findByTurnoCiudad(fechaTurno,idCiudad)
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalFromTurnoCiudad(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno) {
        return profesionalRepository.findFromTurnoCiudad(fechaTurno,idCiudad,idHorarioTurno)
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Mono<Void> asignarProfesionalTurno(
            LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional) {
        return turnoProfesionalesRepository.save(
                new TurnoProfesionalesData()
                        .toBuilder()
                        .fechaTurno(fechaTurno)
                        .idHorarioTurno(idHorarioTurno)
                        .idProfesional(idProfesional)
                        .build()
        ).then();
    }

    @Override
    public Mono<Void> desasignarProfesionalTurno(LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional) {
        return turnoProfesionalesRepository.deleteByFechaTurnoIdHorarioProfesional(
                fechaTurno,idHorarioTurno,idProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalesByIdCiudad(String idCiudad) {
        return profesionalRepository.findByIdCiudad(idCiudad)
                .map(ConverterAgenda :: convertToProfesional);
    }

    public Flux<Tarea> consultarTareasTurnoByProfesional(
            ProfesionalData profesionalData,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad){

        return citaRepository
                .findCitasByTurnoCiudadProfesional(fechaTurno, idHorarioTurno, idCiudad,
                        profesionalData.getNumeroIdentificacion())
                .map(ConverterAgenda :: convertToTarea)
                .map(tarea -> {
                    tarea.setTipo(TIPO_TAREA_VISITA);
                    return tarea;
                })
                .mergeWith(
                        desplazamientoRepository
                                .findByIdCitaPartidaByProfesional(
                                        fechaTurno,idHorarioTurno,idCiudad,profesionalData.getNumeroIdentificacion())
                                .map(ConverterAgenda :: convertToTarea)
                );
    }

    @Override
    public Flux<Actividad> consultarActividadesByProfesionalesCiudadHorarioTurno(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad) {
         return turnoProfesionalesRepository.findTurnoProfesionalByCiudadHorario(
                 fechaTurno,idHorarioTurno,idCiudad)
                 .flatMap(profesionalData -> {
                     Flux<Tarea> tareaFlux = consultarTareasTurnoByProfesional(
                             profesionalData, fechaTurno, idHorarioTurno, idCiudad
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
        return profesionalRepository.insertProfesional(
                profesional.getNumeroIdentificacion(),
                profesional.getIdTipoIdentificacion(),
                profesional.getNombres(),
                profesional.getApellidos(),
                profesional.getFechaNacimiento(),
                profesional.getIdCiudad()
        );
    }

    @Override
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return profesionalRepository.save(ConverterAgenda.convertToProfesionalData(profesional))
                .map(ConverterAgenda :: convertToProfesional);
    }

    @Override
    public Flux<Cita> consultarCitasByTurnoCiudad(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return citaRepository.findCitasByTurnoCiudad(
               fechaTurno,
                idHorarioTurno,
                idCiudad
        ).map(ConverterAgenda :: convertToCita);
    }

    @Override
    public Mono<Void> reprogramarCita(LocalDateTime fechaProgramada, String idCita) {
        return citaRepository.actualizarFechaProgramada(fechaProgramada,idCita)
                .then();
    }

    @Override
    public Mono<Void> agendarToProfesional(String idRemision, String idProfesional) {
        return citaRepository.agendarToProfesional(idRemision,idProfesional);
    }

    @Override
    public Mono<Void> desagendarToProfesional(String idCita) {
        return citaRepository.desagendarToProfesional(idCita);
    }

    @Override
    public Mono<Void> desagendarTurnocompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idCiudad)
                .then(citaRepository.desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idCiudad));
    }

    @Override
    public Mono<Void> autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idCiudad)
                  .then(citaRepository.findCitasByTurnoCiudad(fechaTurno,idHorarioTurno,idCiudad)
                  .collectList()
                  .flatMap(citaDataList -> profesionalRepository.findFromTurnoCiudad(fechaTurno,idCiudad,idHorarioTurno)
                        .collectList()
                        .flatMap(profesionalesDataList -> {
                           AutoAgendador autoAgendador = new AutoAgendador(
                               getOrigenCiudadById(getByIdCiudad(idCiudad)).getCitaGenetic(),
                               ConverterAgenda.convertToListCitaGenetic(citaDataList),
                                      profesionalesDataList.size(),
                                      NUMERO_GENERACIONES,
                                      SIZE_POBLACION_INICIAL,
                                      NUMERO_PADRES_EMPAREJADOS,
                                      PENALIZACION_HOLGURA_NEGATIVA
                               );
                               autoAgendador.run();
                               var mejoResultado = autoAgendador.mejorSolucion();

                               return Flux.fromIterable(profesionalesDataList)
                                   .flatMap(profesionalData -> {
                                      int index = profesionalesDataList.indexOf(profesionalData);
                                      int sizeCitasGen = mejoResultado.getIndividuo().getCitaGen().get(index).size();
                                      return Flux.fromIterable(mejoResultado.getIndividuo()
                                                    .getCitaGen().get(index).subList(1,sizeCitasGen))
                                                    .flatMap(citaGenetic -> {
                                                    String idCita = citaGenetic.getIdCita();
                                                    String idProfesional = profesionalData.getNumeroIdentificacion();
                                                    return citaRepository.agendarToProfesional(idCita, idProfesional);
                                                    });
                                   }).then();
                        })
                   )).then(profesionalRepository.findFromTurnoCiudad(fechaTurno,idCiudad,idHorarioTurno)
                        .collectList()
                        .flatMap(profesionalListData -> Flux.fromIterable(profesionalListData)
                                .flatMap(profesionalData -> calcularDesplazamientoCitaByProfesional(
                                        fechaTurno,
                                        idHorarioTurno,
                                        idCiudad,
                                        profesionalData.getNumeroIdentificacion()
                                ))
                                .then())
                );

    }

    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad){
        return desplazamientoRepository.findByIdCitaPartida(fechaProgramada,idHorarioTurno,idCiudad)
                .map(ConverterAgenda :: converToDesplazamiento);
    }

    @Override
    public Mono<Void> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno,String idCiudad, String idProfesional) {

        return desplazamientoRepository.deleteByFechaTurnoProfesional(fechaTurno,idHorarioTurno,idProfesional)
               .then(citaRepository.findCitasByTurnoCiudadProfesional(fechaTurno,idHorarioTurno,idCiudad,idProfesional)
               .collectList()
               .flatMapMany(Flux::fromIterable)
               .buffer(MAXSIZE,1)
               .filter(citas -> citas.size() == MAXSIZE)
               .flatMap(
                   citas ->{
                      List<DesplazamientoData> desplazamientosList = new ArrayList<>();
                      CitaData citaPartida = citas.get(0);
                      CitaData citaDestino = citas.get(1);
                      DesplazamientoData desplazamientoData = crearDesplazamientoData(citaPartida,citaDestino)
                            .toBuilder().tipo(DESPLAZAMIENTO_VISITA)
                            .idHorarioTurno(idHorarioTurno).duracion(1200).holgura(1200).build();
                         desplazamientosList.add(desplazamientoData);
                         return desplazamientoRepository.saveAll(desplazamientosList);
                         }
                      ).then()
        );
    }

    @Override
    public Flux<Tratamiento> consultarTratamientoByCitas(String idCita) {
        return tratamientoRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToTratamiento);
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
