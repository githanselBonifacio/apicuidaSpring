package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.agenda.Profesional;
import co.com.sura.entity.agenda.Tarea;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.*;
import co.com.sura.postgres.repository.remision.data.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Repository
public class AgendaRepositoryAdapter implements AgendaRepository {


    @Autowired
    private ProfesionalRepository profesionalRepository;

    @Autowired
    private TurnoProfesionalesRepository turnoProfesionalesRepository;
    @Autowired
    private CitaRepository citaRepository;

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

    @Override
    public Flux<Actividad> consultarActividadesByProfesionalesCiudadHorarioTurno(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad) {
         return turnoProfesionalesRepository.findTurnoProfesionalByCiudadHorario(
                 fechaTurno,idHorarioTurno,idCiudad)
                 .flatMap(profesionalData -> {
                     Flux<CitaData> citaDataFlux = citaRepository
                             .findCitasByTurnoCiudadProfesional(
                                     fechaTurno,
                                     idHorarioTurno,
                                     idCiudad,
                                     profesionalData.getNumeroIdentificacion()
                             );
                     return  citaDataFlux.collectList()
                             .map(citas ->{
                                 List<Tarea> tareaList = citas.stream()
                                         .map(ConverterAgenda :: convertToTarea)
                                         .peek(tarea -> tarea.setTipo("visita"))
                                         .collect(Collectors.toList());
                                 return ConverterAgenda.convertToActividad(profesionalData)
                                         .toBuilder()
                                         .tareas(tareaList)
                                         .build();
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
    public Mono<Void> agendarToProfesional(String idRemision, String idProfesional) {
        return citaRepository.agendarToProfesional(idRemision,idProfesional);
    }

    @Override
    public Mono<Void> desagendarToProfesional(String idCita) {
        return citaRepository.desagendarToProfesional(idCita);
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
