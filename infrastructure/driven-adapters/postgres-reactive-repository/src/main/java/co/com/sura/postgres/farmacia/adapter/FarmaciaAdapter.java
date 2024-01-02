package co.com.sura.postgres.farmacia.adapter;

import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.farmacia.gateway.FarmaciaRepository;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.remision.repository.datospaciente.PacienteRepository;
import co.com.sura.postgres.remision.repository.procedimientos.SoporteNutricionalRepository;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;

import static co.com.sura.farmacia.entity.TipoNotificacionFarmacia.APLICACION_MEDICAMENTO;
import static co.com.sura.farmacia.entity.TipoNotificacionFarmacia.SOPORTE_NUTRICIONAL;

@Repository
public class FarmaciaAdapter implements FarmaciaRepository {

    private final PacienteRepository pacienteRepository;
    private final SoporteNutricionalRepository soporteNutricionalRepository;
    private final TratamientoRepository tratamientoRepository;

    @Autowired
    public FarmaciaAdapter(PacienteRepository pacienteRepository,
                           SoporteNutricionalRepository soporteNutricionalRepository,
                           TratamientoRepository tratamientoRepository) {
        this.pacienteRepository = pacienteRepository;
        this.soporteNutricionalRepository = soporteNutricionalRepository;
        this.tratamientoRepository = tratamientoRepository;
    }

    //farmacia
    @Override
    public Flux<PacienteTratamientoCita> consultarAllPacienteWithMedicamentosToFarmacia() {
      return pacienteRepository.findAllTratamientosPacientes(
              EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado(),EstadosCita.FINALIZADA.getEstado())
              .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(APLICACION_MEDICAMENTO.getTipo());
                            return pacienteTratamientoCita;
                        })
              .mergeWith(pacienteRepository.findAllSoporteNutricionalPacientes(
              EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado(),EstadosCita.FINALIZADA.getEstado()
                        )
                        .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(SOPORTE_NUTRICIONAL.getTipo());
                            return pacienteTratamientoCita;
                        }))
                .sort(Comparator.comparing(PacienteTratamientoCita::getNotificado).reversed()
                .thenComparing(PacienteTratamientoCita::getFechaProgramada).reversed());

    }

    @Override
    public Flux<PacienteTratamientoCita> consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
            LocalDate fechaTurno, String idRegional,Integer idHorarioTurno) {
        return pacienteRepository.findAllTratamientosPacientesByTurnoRegionalHorario(
                        fechaTurno,idRegional,idHorarioTurno,
                        EstadosCita.CONFIRMADA.getEstado(),
                        EstadosCita.EN_PROGRESO.getEstado(),
                        EstadosCita.FINALIZADA.getEstado())

                .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(APLICACION_MEDICAMENTO.getTipo());
                            return pacienteTratamientoCita;
                        }
                ).mergeWith(pacienteRepository.findAllSoporteNutricionalPacientesByTurnoRegionalHorario(
                        fechaTurno,idRegional,idHorarioTurno,
                        EstadosCita.CONFIRMADA.getEstado(),
                        EstadosCita.EN_PROGRESO.getEstado(),
                        EstadosCita.FINALIZADA.getEstado())
                        .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(SOPORTE_NUTRICIONAL.getTipo());
                            return pacienteTratamientoCita;
                        })
                ).sort(Comparator.comparing(PacienteTratamientoCita::getNotificado).reversed()
                        .thenComparing(PacienteTratamientoCita::getFechaProgramada).reversed());
    }

    @Override
    public Mono<Boolean> notificarMedicamentosToFarmacia(List<PacienteTratamientoCita> tratamientoCitasList) {
        return Flux.fromIterable(tratamientoCitasList)
                .flatMap(pacienteTratamientoCita -> {
                    Mono<Void> notificarTratamiento= Mono.empty();
                    Mono<Void> notificarSoporteNotricional= Mono.empty();
                    if (pacienteTratamientoCita.getIdTratamiento() != null) {
                         notificarTratamiento =  tratamientoRepository.updateNotificar(pacienteTratamientoCita.getIdTratamiento());
                    } else if (pacienteTratamientoCita.getIdSoporteNutricional() != null) {
                         notificarSoporteNotricional= soporteNutricionalRepository
                                .updateNotificar(pacienteTratamientoCita.getIdSoporteNutricional());
                    }
                    return notificarTratamiento
                            .then(notificarSoporteNotricional);
                })
                .then(Mono.just(Boolean.TRUE));
    }
}
