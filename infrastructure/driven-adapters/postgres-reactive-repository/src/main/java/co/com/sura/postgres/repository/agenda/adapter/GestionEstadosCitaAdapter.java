package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.entity.agenda.GestionEstadosCitasRepository;
import co.com.sura.exception.ErrorEstadoCitaNoValido;
import co.com.sura.exception.ExceptionNegocio;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.genericos.Numeros;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Repository
public class GestionEstadosCitaAdapter implements GestionEstadosCitasRepository {

    private final CitaRepository citaRepository;
    private final AgendamientoAutomaticoAdapter agendamientoAutomaticoAdapter;
    private final DesplazamientoRepository desplazamientoRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;

    @Autowired
    public GestionEstadosCitaAdapter(CitaRepository citaRepository,
                                     AgendamientoAutomaticoAdapter agendamientoAutomaticoAdapter,
                                     DesplazamientoRepository desplazamientoRepository,
                                     HorarioTurnoRepository horarioTurnoRepository) {
        this.citaRepository = citaRepository;
        this.agendamientoAutomaticoAdapter = agendamientoAutomaticoAdapter;
        this.desplazamientoRepository = desplazamientoRepository;

        this.horarioTurnoRepository = horarioTurnoRepository;
    }
    private Mono<Boolean> validarReprogramacionCitaEnHorarioTurno(CitaData cita){
        return Mono.just(cita)
                .flatMap(citaData -> horarioTurnoRepository.findById(cita.getIdHorarioTurno())
                   .map(horarioTurnoData ->
                          HorarioTurnoData
                                 .validarHorarioCita(cita.getFechaProgramada(),horarioTurnoData, cita.getDuracion())));
    }

    @Override
    public Mono<Boolean> agendarToProfesional(
            String idCita, String idProfesional, LocalDateTime fechaProgramada,
            Integer idHorarioTurno, String idRegional) {

       return citaRepository.findById(idCita)
          .switchIfEmpty(Mono.error(new ExceptionNegocio(Mensajes.CITA_NO_EXISTE.getValue())))
          .flatMap(citaData -> Mono.zip(
                               validarDisponibilidadFechaCita(citaData,idProfesional),
                               validarReprogramacionCitaEnHorarioTurno(citaData)))

          .flatMap(tupleValidacion ->{
             if(Boolean.FALSE.equals(tupleValidacion.getT1())){
                 return Mono.error(new ExceptionNegocio(Mensajes.ERROR_FECHA_CITA.getValue()));

             }else if(Boolean.FALSE.equals(tupleValidacion.getT2())) {
                 return Mono.error(new ExceptionNegocio(Mensajes.ERROR_FECHA_CITA_HORARIO.getValue()));

             }else{
                return citaRepository.updateEstadoAndProfesional(idCita,EstadosCita.AGENDADA.getEstado(),idProfesional)
                       .then(agendamientoAutomaticoAdapter.insertDesplazamientoCitaByProfesional(
                                           fechaProgramada.toLocalDate(), idHorarioTurno, idRegional, idProfesional));
             }
          });

    }
    private Mono<Boolean> validarDisponibilidadFechaCita(CitaData cita,String idProfesional){
        return Mono.just(cita)
           .flatMap(citaData -> Mono.zip(

             citaRepository.findCitaMasCercanaAnterior(
                                citaData.getFechaProgramada(),citaData.getIdCita(),
                                citaData.getIdHorarioTurno(),citaData.getIdRegional(), idProfesional)
                                .defaultIfEmpty(CitaData.builder().idCita("noCita").build()),

             citaRepository.findCitaMasCercanaPosterior(
                                citaData.getFechaProgramada(), citaData.getIdCita(),
                                citaData.getIdHorarioTurno(),citaData.getIdRegional(), idProfesional)
                               .defaultIfEmpty(CitaData.builder().idCita("noCita").build()),

                         Mono.just(citaData)))
           .flatMap(citasTuple-> Mono.zip(
             desplazamientoRepository.findByIdCitaPartida(citasTuple.getT1().getIdCita())
                 .defaultIfEmpty(DesplazamientoData.builder().duracion(0).build()),

             desplazamientoRepository.findByIdCitaPartida(citasTuple.getT3().getIdCita())
                .defaultIfEmpty(DesplazamientoData.builder().duracion(Numeros.NOVECIENTOS_SEGUNDOS.getValue()).build()),

            desplazamientoRepository.findBySede(
                    cita.getFechaProgramada(),cita.getIdRegional(),idProfesional,cita.getIdHorarioTurno()))

           .map(despTuple->CitaData.validarDisponibilidadFechasToAgendar(
                                    citasTuple.getT1(),citasTuple.getT2(),citasTuple.getT3(),
                                    despTuple.getT1(),despTuple.getT2(),despTuple.getT3()
           )));
    }
    @Override
    public Mono<Boolean> desagendarToProfesional(
            String idCita,String idProfesional,LocalDate fechaTurno,Integer idHorarioTurno,String idRegional) {
        return citaRepository.updateEstadoAndProfesional(idCita,EstadosCita.SIN_AGENDAR.getEstado(),null)
                .then(agendamientoAutomaticoAdapter
                        .insertDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }



    private Mono<Boolean> actualizarEstadoCita(String idCita,EstadosCita estadoRequisito,EstadosCita estadoNuevo){
        return citaRepository.findById(idCita)
                .switchIfEmpty(Mono.error(new ExceptionNegocio(Mensajes.CITA_NO_EXISTE.getValue())))
                .map(CitaData::getIdEstado)
                .map(idEstadoCita -> idEstadoCita.equals(estadoRequisito.getEstado()))
                .flatMap(validacionEstado -> {
                   if(Boolean.TRUE.equals(validacionEstado)){
                      return citaRepository.updateEstado(idCita,estadoNuevo.getEstado());
                   }else{
                      return Mono.error(
                              new ErrorEstadoCitaNoValido(Mensajes.ERROR_ESTADO_CITA.getValue()+
                                      EstadosCita.getNombreEstado(estadoRequisito)));
                   }
                })
                .onErrorResume(Mono::error)
                .then(Mono.just(Boolean.TRUE));
    }
    @Override
    public Mono<Boolean> confirmarCita(String idCita) {
        return actualizarEstadoCita(idCita,EstadosCita.AGENDADA,EstadosCita.CONFIRMADA);

    }

    @Override
    public Mono<Boolean> iniciarAtencionCita(String idCita) {
        return actualizarEstadoCita(idCita,EstadosCita.CONFIRMADA,EstadosCita.EN_PROGRESO);
    }

    @Override
    public Mono<Boolean> finalizarAtencionCita(String idCita) {
        return actualizarEstadoCita(idCita,EstadosCita.EN_PROGRESO,EstadosCita.FINALIZADA);
    }
}
