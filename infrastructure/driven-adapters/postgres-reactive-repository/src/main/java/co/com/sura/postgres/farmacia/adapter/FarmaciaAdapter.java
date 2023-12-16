package co.com.sura.postgres.farmacia.adapter;

import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.farmacia.gateway.FarmaciaRepository;
import co.com.sura.postgres.remision.repository.PacienteRepository;
import co.com.sura.postgres.remision.repository.SoporteNutricionalRepository;
import co.com.sura.postgres.remision.repository.TratamientoRepository;
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
        return pacienteRepository.findAllTratamientosPacientes()
                .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(APLICACION_MEDICAMENTO.getTipo());
                            return pacienteTratamientoCita;
                        }
                ).mergeWith(pacienteRepository.findAllSoporteNutricionalPacientes()
                        .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(SOPORTE_NUTRICIONAL.getTipo());
                            return pacienteTratamientoCita;
                        })
                ).sort(Comparator.comparing(PacienteTratamientoCita::getNotificado).reversed()
                        .thenComparing(PacienteTratamientoCita::getFechaProgramada).reversed());

    }

    @Override
    public Flux<PacienteTratamientoCita> consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
            LocalDate turno, Integer idHorario, String idRegional) {
        return pacienteRepository.findAllTratamientosPacientesByTurnoRegionalHorario(turno,idHorario,idRegional)
                .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(APLICACION_MEDICAMENTO.getTipo());
                            return pacienteTratamientoCita;
                        }
                ).mergeWith(pacienteRepository.findAllSoporteNutricionalPacientesByTurnoRegionalHorario(
                                turno,idHorario,idRegional)
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
                    Mono<Void> tratamientoUpdate = Mono.empty();
                    if (pacienteTratamientoCita.getIdTratamiento() != null) {
                        tratamientoUpdate = tratamientoRepository
                                .updateNotificar(pacienteTratamientoCita.getIdTratamiento());
                    }
                    return tratamientoUpdate;
                })
                .thenMany(Flux.fromIterable(tratamientoCitasList)
                        .flatMap(pacienteProcedimientoCita -> {
                            Mono<Void> soporteNutricionalUpdate = Mono.empty();
                            if (pacienteProcedimientoCita.getIdSoporteNutricional() != null) {
                                soporteNutricionalUpdate = soporteNutricionalRepository
                                        .updateNotificar(pacienteProcedimientoCita.getIdSoporteNutricional());
                            }
                            return soporteNutricionalUpdate;
                        }))
                .then(Mono.just(Boolean.TRUE));
    }
}
