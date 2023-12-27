package co.com.sura.postgres.agenda.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.agenda.gateway.GestionEstadosCitasRepository;
import co.com.sura.exception.ErrorEstadoCitaNoValido;
import co.com.sura.exception.ExceptionNegocio;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.genericos.Numeros;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.reportes.data.RegistroCancelacionCitaData;
import co.com.sura.postgres.reportes.repository.RegistroCancelacionCitaRepository;
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

    private final RegistroCancelacionCitaRepository registroCancelacionCitaRepository;

    @Autowired
    public GestionEstadosCitaAdapter(CitaRepository citaRepository,
                                     AgendamientoAutomaticoAdapter agendamientoAutomaticoAdapter,
                                     DesplazamientoRepository desplazamientoRepository,
                                     HorarioTurnoRepository horarioTurnoRepository,
                                     RegistroCancelacionCitaRepository registroCancelacionCitaRepository) {
        this.citaRepository = citaRepository;
        this.agendamientoAutomaticoAdapter = agendamientoAutomaticoAdapter;
        this.desplazamientoRepository = desplazamientoRepository;

        this.horarioTurnoRepository = horarioTurnoRepository;
        this.registroCancelacionCitaRepository = registroCancelacionCitaRepository;
    }

    @Override
    public Mono<Boolean> agendarToProfesional(
            String idCita, String idProfesional, LocalDateTime fechaProgramada,
            Integer idHorarioTurno, String idRegional) {

       return citaRepository.findById(idCita)
          .switchIfEmpty(Mono.error(new ExceptionNegocio(Mensajes.CITA_NO_EXISTE)))
          .flatMap(citaData -> Mono.zip(
                               this.validarDisponibilidadFechaCita(citaData,idProfesional),
                               this.validarReprogramacionCitaEnHorarioTurno(citaData)))


          .flatMap(tupleValidacion ->{
             if(Boolean.FALSE.equals(tupleValidacion.getT1())){
                 return Mono.error(new ExceptionNegocio(Mensajes.ERROR_FECHA_CITA));

             }else if(Boolean.FALSE.equals(tupleValidacion.getT2())) {
                 return Mono.error(new ExceptionNegocio(Mensajes.ERROR_FECHA_CITA_HORARIO));

             }else{
                return this.uptateEstadoProfesional(idCita,idProfesional,fechaProgramada,idHorarioTurno,idRegional);
             }
          });

    }
    @Override
    public Mono<Boolean> desagendarToProfesional(
            String idCita,String idProfesional,LocalDate fechaTurno,Integer idHorarioTurno,String idRegional) {
        return citaRepository.updateEstadoAndProfesional(idCita,EstadosCita.SIN_AGENDAR.getEstado(),null)
                .then(agendamientoAutomaticoAdapter
                        .insertDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> confirmarCita(String idCita) {
        return this.actualizarEstadoCita(idCita,EstadosCita.AGENDADA,EstadosCita.CONFIRMADA);

    }

    @Override
    public Mono<Boolean> iniciarAtencionCita(String idCita) {
        return this.actualizarEstadoCita(idCita,EstadosCita.CONFIRMADA,EstadosCita.EN_PROGRESO);
    }

    @Override
    public Mono<Boolean> finalizarAtencionCita(String idCita) {
        return this.actualizarEstadoCita(idCita,EstadosCita.EN_PROGRESO,EstadosCita.FINALIZADA);
    }

    @Override
    public Mono<Boolean> cancelarCita(String idCita, Integer idMotivoCancelacion) {
        return citaRepository.findById(idCita)
                .switchIfEmpty(Mono.error(new ExceptionNegocio(Mensajes.CITA_NO_EXISTE)))
                .flatMap(citaData -> Mono.zip(
                                    Mono.just(  citaData.getIdEstado()==EstadosCita.AGENDADA.getEstado() ||
                                        citaData.getIdEstado()==EstadosCita.SIN_AGENDAR.getEstado()),
                                    Mono.just(citaData)))

                .flatMap(tupla->{
                    if (Boolean.TRUE.equals(tupla.getT1())){
                        return citaRepository.updateEstado(idCita,EstadosCita.CANCELADA.getEstado())
                                .then(registroCancelacionCitaRepository.save(
                                        RegistroCancelacionCitaData.builder()
                                                .idCita(idCita)
                                                .fechaTurno(tupla.getT2().getFechaProgramada().toLocalDate())
                                                .idMotivoCancelacion(idMotivoCancelacion)
                                                .build()))
                                .then(Mono.just(Boolean.TRUE));
                    }else{
                        return Mono.error(
                                new ErrorEstadoCitaNoValido(Mensajes.ERROR_ESTADO_CITA+
                                        EstadosCita.getNombreEstado(EstadosCita.AGENDADA)+" o "+
                                        EstadosCita.getNombreEstado(EstadosCita.SIN_AGENDAR)));
                    }
                });

    }

    private Mono<Boolean> validarReprogramacionCitaEnHorarioTurno(CitaData cita){
        return Mono.just(cita)
                .flatMap(citaData -> horarioTurnoRepository.findById(cita.getIdHorarioTurno())
                        .map(horarioTurnoData ->
                                HorarioTurnoData
                                        .validarHorarioCita(cita.getFechaProgramada(),horarioTurnoData, cita.getDuracion())))
                .onErrorResume(e->Mono.error(new Exception(e.getMessage())));
    }
    private Mono<Boolean> uptateEstadoProfesional( String idCita, String idProfesional, LocalDateTime fechaProgramada,
                                                   Integer idHorarioTurno, String idRegional){

        return citaRepository.updateEstadoAndProfesional(idCita,EstadosCita.AGENDADA.getEstado(),idProfesional)
                .then(agendamientoAutomaticoAdapter.insertDesplazamientoCitaByProfesional(
                        fechaProgramada.toLocalDate(), idHorarioTurno, idRegional, idProfesional));
    }
    private Mono<Boolean> validarDisponibilidadFechaCita(CitaData cita,String idProfesional){
        return Mono.just(cita)
           .flatMap(citaData -> Mono.zip(

             citaRepository.findMasCercanaAnterior(
                                citaData.getFechaProgramada(),citaData.getIdCita(),
                                citaData.getIdHorarioTurno(),citaData.getIdRegional(), idProfesional)
                                .defaultIfEmpty(CitaData.builder().idCita("noCita").build()),

             citaRepository.findMasCercanaPosterior(
                                citaData.getFechaProgramada(), citaData.getIdCita(),
                                citaData.getIdHorarioTurno(),citaData.getIdRegional(), idProfesional)
                               .defaultIfEmpty(CitaData.builder().idCita("noCita").build()),

                         Mono.just(citaData)))

           .flatMap(citasTuple-> Mono.zip(
             desplazamientoRepository.findByIdCitaPartida(citasTuple.getT1().getIdCita())
                 .defaultIfEmpty(DesplazamientoData.builder().duracion(0).build()),

             desplazamientoRepository.findByIdCitaPartida(citasTuple.getT3().getIdCita())
                .defaultIfEmpty(DesplazamientoData.builder().duracion(Numeros.NOVECIENTOS_SEGUNDOS).build()),

            desplazamientoRepository.findBySede(
                    cita.getFechaProgramada(),cita.getIdRegional(),idProfesional,cita.getIdHorarioTurno())
                    .defaultIfEmpty(DesplazamientoData.builder().build()))
           .map(despTuple->CitaData.validarDisponibilidadFechasToAgendar(citasTuple.getT1(),citasTuple.getT2(),
                   citasTuple.getT3(), despTuple.getT1(),despTuple.getT2(),despTuple.getT3())));
    }

    private Mono<Boolean> actualizarEstadoCita(String idCita,EstadosCita estadoRequisito,EstadosCita estadoNuevo){
        return citaRepository.findById(idCita)
                .switchIfEmpty(Mono.error(new ExceptionNegocio(Mensajes.CITA_NO_EXISTE)))
                .map(citaData -> citaData.getIdEstado().equals(estadoRequisito.getEstado()))
                .flatMap(validacionEstado -> {
                   if(Boolean.TRUE.equals(validacionEstado)){
                      return citaRepository.updateEstado(idCita,estadoNuevo.getEstado());
                   }else{
                      return Mono.error(
                              new ErrorEstadoCitaNoValido(Mensajes.ERROR_ESTADO_CITA+
                                      EstadosCita.getNombreEstado(estadoRequisito)));
                   }
                })
                .onErrorResume(Mono::error)
                .then(Mono.just(Boolean.TRUE));
    }

}
