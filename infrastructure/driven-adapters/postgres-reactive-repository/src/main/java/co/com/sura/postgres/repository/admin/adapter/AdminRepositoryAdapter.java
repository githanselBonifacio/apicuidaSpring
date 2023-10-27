package co.com.sura.postgres.repository.admin.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.admin.*;
import co.com.sura.entity.agenda.*;
import co.com.sura.genericos.GenericosFactory;
import co.com.sura.genericos.Resultado;
import co.com.sura.postgres.repository.admin.data.*;
import co.com.sura.postgres.repository.agenda.data.*;
import co.com.sura.postgres.repository.maestros.adapter.ConverterMaestros;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static co.com.sura.constantes.Mensajes.*;
import static co.com.sura.entity.admin.TipoNotificacionFarmacia.APLICACION_MEDICAMENTO;
import static co.com.sura.entity.admin.TipoNotificacionFarmacia.SOPORTE_NUTRICIONAL;


@Repository
public class AdminRepositoryAdapter implements RemisionCrudRepository {

    private final RemisionRepository remisionRepository;
    private final UbicacionRepository ubicacionRepository;
    private final PacienteRepository pacienteRepository;
    private final DatosAtencionPacienteRepository datosAtencionPacienteRepository;
    private final RemisionDiagnosticoRepository remisionDiagnosticoRepository;
    private final CitaRepository citaRepository;
    private final TratamientoRepository tratamientoRepository;
    private final CanalizacionRepository canalizacionRepository;
    private final SecrecionRepository secrecionRepository;
    private final CuracionRepository curacionRepository;
    private final FototerapiaRepository fototerapiaRepository;
    private final SondajeRepository sondajeRepository;
    private final TomaMuestraRepository tomaMuestraRepository;
    private final SoporteNutricionalRepository soporteNutricionalRepository;
    private final RegistroHistorialRepository registroHistorialRemisionRepository;
    private final ProfesionalRepository profesionalRepository;
    private final MovilRepository movilRepository;
    private final ConductorRepository conductorRepository;
    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final ItemSecuenciaTurnoRepository secuenciaTurnoRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;
    @Autowired
    public AdminRepositoryAdapter(
            RemisionRepository remisionRepository, UbicacionRepository ubicacionRepository,
            PacienteRepository pacienteRepository, DatosAtencionPacienteRepository datosAtencionPacienteRepository,
            RemisionDiagnosticoRepository remisionDiagnosticoRepository, CitaRepository citaRepository,
            TratamientoRepository tratamientoRepository, CanalizacionRepository canalizacionRepository,
            SecrecionRepository secrecionRepository, CuracionRepository curacionRepository,
            FototerapiaRepository fototerapiaRepository, SondajeRepository sondajeRepository,
            TomaMuestraRepository tomaMuestraRepository, SoporteNutricionalRepository soporteNutricionalRepository,
            RegistroHistorialRepository registroHistorialRemisionRepository,
            ProfesionalRepository profesionalRepository, MovilRepository movilRepository,
            ConductorRepository conductorRepository, TurnoProfesionalesRepository turnoProfesionalesRepository,
            ItemSecuenciaTurnoRepository secuenciaTurnoRepository, HorarioTurnoRepository horarioTurnoRepository) {

        this.remisionRepository = remisionRepository;
        this.ubicacionRepository = ubicacionRepository;
        this.pacienteRepository = pacienteRepository;
        this.datosAtencionPacienteRepository = datosAtencionPacienteRepository;
        this.remisionDiagnosticoRepository = remisionDiagnosticoRepository;
        this.citaRepository = citaRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.canalizacionRepository = canalizacionRepository;
        this.secrecionRepository = secrecionRepository;
        this.curacionRepository = curacionRepository;
        this.fototerapiaRepository = fototerapiaRepository;
        this.sondajeRepository = sondajeRepository;
        this.tomaMuestraRepository = tomaMuestraRepository;
        this.soporteNutricionalRepository = soporteNutricionalRepository;
        this.registroHistorialRemisionRepository = registroHistorialRemisionRepository;
        this.profesionalRepository = profesionalRepository;
        this.movilRepository = movilRepository;
        this.conductorRepository = conductorRepository;
        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.secuenciaTurnoRepository = secuenciaTurnoRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
    }

    @Override
    public Flux<Remision> consultarRemisiones() {
        return remisionRepository.findAllRemision();
    }

    //Crear y actualizar remisiones

    protected Mono<Void> registrarPacienteRemision(RemisionRequest remisionRequest, boolean esNovedad){
        var ubicacionData = ConverterAdmin.extraerUbicacionData(remisionRequest);
        var pacienteData = ConverterAdmin.extraerPacienteData(remisionRequest);

        if(esNovedad){
            return Mono.from(ubicacionRepository.save(ubicacionData))
                    .then(Mono.from(pacienteRepository.save(pacienteData)))
                    .then();
        }else {
            return Mono.from(pacienteRepository.existsById(pacienteData.getNumeroIdentificacion()))
                    .flatMap(pacienteExiste ->{
                        if (Boolean.TRUE.equals(pacienteExiste)){
                            return ubicacionRepository.save(ubicacionData)
                                    .then(Mono.from(pacienteRepository.save(pacienteData))).then();
                        }else{
                            return ubicacionRepository.insertNuevaUbicacion(ubicacionData.getIdUbicacion())
                                    .then(Mono.from(ubicacionRepository.save(ubicacionData)))
                                    .then(Mono.from(pacienteRepository
                                                    .insertNuevoPaciente(pacienteData.getNumeroIdentificacion())))
                                    .then(Mono.from(pacienteRepository.save(pacienteData))).then();
                        }
                    });
        }
    }
    protected  Mono<Void> registrarPlanManejo(RemisionRequest remisionRequest, List<CitaRequest> citasRequest,
                                              NovedadRequest novedadRequest){
        var counter = new AtomicInteger(
                citaRepository.findLastNumberCitaRemision(remisionRequest.getIdRemision())
                        .blockOptional()
                        .orElse(0) + 1);

        if(novedadRequest.getFechaAplicarNovedad() !=null){
            citasRequest.removeIf(
                    citaRequest -> citaRequest.getFechaInicio().isBefore(novedadRequest.getFechaAplicarNovedad()));
        }


        citasRequest.forEach(citaRequest -> {
            String newIdCita = remisionRequest.getIdRemision() + "-" + counter;
            citaRequest.setIdCita(newIdCita);
            counter.getAndIncrement();
        });

        List<CitaData> citasData = ConverterAdmin.convertirCitasDataList(citasRequest, remisionRequest);
        List<TratamientoData> tratamientosData = ConverterAdmin.extraerTratamientoData(citasRequest);
        List<CanalizacionData> canalizacionesData = ConverterAdmin.extraerCanalizacionData(citasRequest);
        List<FototerapiaData> fototerapiaData = ConverterAdmin.extraerFototerapiaData(citasRequest);
        List<SecrecionData> secrecionData = ConverterAdmin.extraerSecrecionData(citasRequest);
        List<SondajeData> sondajeData = ConverterAdmin.extraerSondajeData(citasRequest);
        List<SoporteNutricionalData> soporteNutricionalData = ConverterAdmin
                .extraerSoporteNutricionalData(citasRequest);

        List<TomaMuestraData> tomaMuestraData = ConverterAdmin.extraerSoporteTomaMuestraData(citasRequest);
        List<CuracionData> curacionesData = ConverterAdmin.extraerCuracionData(citasRequest);

        Flux<TratamientoData> tratamientosFlux         = tratamientoRepository.saveAll(tratamientosData);
        Flux<CanalizacionData> canalizacionDataFlux    = canalizacionRepository.saveAll(canalizacionesData);
        Flux<FototerapiaData> fototerapiaDataFlux      = fototerapiaRepository.saveAll(fototerapiaData);
        Flux<SecrecionData> secrecionDataFlux          = secrecionRepository.saveAll(secrecionData);
        Flux<SondajeData> sondajeDataFlux              = sondajeRepository.saveAll(sondajeData);
        Flux<SoporteNutricionalData> soporteNutricionalDataFlux = soporteNutricionalRepository
                .saveAll(soporteNutricionalData);

        Flux<TomaMuestraData> tomaMuestraDataFlux = tomaMuestraRepository.saveAll(tomaMuestraData);
        Flux<CuracionData> curacionDataFlux = curacionRepository.saveAll(curacionesData);

        return  Mono.from(Mono.from(citaRepository.insertMultiplescitas(citasData)))
                .then(tratamientosFlux.collectList())
                .then(canalizacionDataFlux.collectList())
                .then(fototerapiaDataFlux.collectList())
                .then(secrecionDataFlux.collectList())
                .then(sondajeDataFlux.collectList())
                .then(soporteNutricionalDataFlux.collectList())
                .then(tomaMuestraDataFlux.collectList())
                .then(curacionDataFlux.collectList())
                .then();

    }
    protected  Mono<Void> registrarDatosRemision(RemisionRequest remisionRequest, boolean esNovedad){
        var remisionData = ConverterAdmin.convertToRemisionRequest(remisionRequest);
        var datosAtencionPacienteData = ConverterAdmin
                .extraerDatosAtencionPacienteData(
                        remisionRequest.getDatosAtencionPaciente(), remisionRequest.getIdRemision());

        List<RemisionDiagnosticoData> diagnosticosData = ConverterAdmin
                .extraerRemisionDiagnosticoData(remisionRequest.getDiagnosticos(), remisionRequest.getIdRemision());

       if(esNovedad){
           return   Mono.from(Mono.from(remisionRepository.save(remisionData)))
                   .then(Mono.from(datosAtencionPacienteRepository.updateDatosAtencion(datosAtencionPacienteData)))
                   .then(Mono.from(remisionDiagnosticoRepository.updateMultiplesDiagnosticos(diagnosticosData)))
                   .then();

       }else {
           return  Mono.from(Mono.from(remisionRepository.insertNuevaRemision(remisionData.getIdRemision())))
                   .then(Mono.from(Mono.from(remisionRepository.save(remisionData))))
                   .then(Mono.from(datosAtencionPacienteRepository.save(datosAtencionPacienteData)))
                   .then(Mono.from(remisionDiagnosticoRepository.insertMultiplesDiagnosticos(diagnosticosData)))
                   .then();
       }
    }

    @Override
    public Mono<Boolean> crearRemisionCita(RemisionRequest remisionRequest, List<CitaRequest> citasRequest) {

        String idRemision = remisionRequest.getIdRemision();
        Mono<Boolean> validarRemision = remisionRepository.existsById(idRemision);
        validarRemision.subscribe();

        if(validarRemision.blockOptional().orElse(false)){
            return Mono.error(new Throwable(Mensajes.REMISION_EXISTENTE.getValue().replace("?",idRemision)));
        }

        String numeroIdentificacionPaciente = remisionRequest.getNumeroIdentificacion();
        return registrarPacienteRemision(remisionRequest,false)
                    .then(registrarDatosRemision(remisionRequest,false))
                    .then(registrarPlanManejo(remisionRequest,citasRequest,new NovedadRequest()))
                    .onErrorMap(throwable -> {
                       Mono<Void>  error = Mono.from(
                           remisionRepository.deleteAllDataRemision(idRemision,numeroIdentificacionPaciente));
                       error.subscribe();
                       return new Exception(throwable.getMessage());
                    })
                .then(Mono.just(true));
    }
    @Override
    public Mono<Boolean> actualizarRemisionPorNovedad(RemisionRequest remisionRequest, List<CitaRequest> citasRequest,
                                                   NovedadRequest novedadRequest) {

        String idRemision = remisionRequest.getIdRemision();
        Mono<Boolean> validarRemision = remisionRepository.validarEstadosRemisionToEgreso(idRemision);
        validarRemision.subscribe();

        if (!validarRemision.blockOptional().orElse(false)) {
            return Mono.error(new Throwable(REMISION_NO_EXISTENTE.getValue().replace("?", idRemision)));
        }

        Mono<RegistroHistorialRemisionData> registroRemision = Mono.from(
            registroHistorialRemisionRepository
                .buildByIdRemisionForUpdate(remisionRequest.getIdRemision(), novedadRequest.getFechaAplicarNovedad()));
        registroRemision.subscribe();

        var registroRemisionData = registroRemision.blockOptional().orElse(new RegistroHistorialRemisionData());
        registroRemisionData.setMotivoNovedad(novedadRequest.getMotivoNovedad());
        registroRemisionData.setFechaAplicacionNovedad(novedadRequest.getFechaAplicarNovedad());
        registroRemisionData.setFechaRegistro(LocalDateTime.now());

        if (citasRequest == null) {
            registroRemisionData.setCitas(null);
            return Mono.from(registroHistorialRemisionRepository.save(registroRemisionData))
                    .then(Mono.from(citaRepository
                            .deleteCitaDataByIdRemision(idRemision, novedadRequest.getFechaAplicarNovedad())))
                    .then(registrarPacienteRemision(remisionRequest, true))
                    .then(registrarDatosRemision(remisionRequest, true))
                    .then(Mono.just(Boolean.TRUE));
        } else {
            return Mono.from(registroHistorialRemisionRepository.save(registroRemisionData))
                    .then(Mono.from(citaRepository
                            .deleteCitaDataByIdRemision(idRemision, novedadRequest.getFechaAplicarNovedad())))
                    .then(registrarPacienteRemision(remisionRequest, true))
                    .then(registrarDatosRemision(remisionRequest, true))
                    .then(registrarPlanManejo(remisionRequest, citasRequest, novedadRequest))
                    .then(Mono.just(Boolean.TRUE));
        }

    }

    @Override
    public Mono<Boolean> egresarRemisionById(String idRemision) {
        return remisionRepository.validarEstadosRemisionToEgreso(idRemision)
           .flatMap(exists -> {
               if (Boolean.FALSE.equals(exists)) {
                 return Mono.error(new Throwable(REMISION_NO_EXISTENTE.getValue().replace("?", idRemision)));
               }
               return citaRepository.validarEstadosCitasToEgreso(idRemision)
               .flatMap(valid -> {if (Boolean.TRUE.equals(valid)) {
                   return Mono.error(new Throwable(REMISION_CITAS_PROGRESO.getValue().replace("?", idRemision)));
               }
               return citaRepository.cancelarCitasForEgresoRemision(idRemision)
                       .then(remisionRepository.egresarRemisionById(idRemision));
               });
           }).then(Mono.just(true));
    }

    //datos paciente
    @Override
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByIdRemision(String idRemision) {
        return datosAtencionPacienteRepository.findByIdRemision(idRemision)
                .map(ConverterAdmin:: convertToDatosAtencionPaciente);
    }

    @Override
    public Mono<Paciente> consultarPacienteFromRemision(String idRemision) {
        return pacienteRepository.findPacienteByNumeroIdRemision(idRemision)
                .flatMap(pacienteData -> ubicacionRepository.findById(pacienteData.getIdUbicacion())
                        .map(ubicacionData -> {
                            var paciente = ConverterAdmin.convertToPaciente(pacienteData);
                            paciente.setUbicacion(ConverterAdmin.convertToUbicacion(ubicacionData));
                            return paciente;
                        }));
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
              Mono<Void> tratamientoUpdate = pacienteTratamientoCita.getIdTratamiento() != null ?
                 tratamientoRepository.updateNotificar(pacienteTratamientoCita.getIdTratamiento()) : Mono.empty();

                Mono<Void> soporteNutricionalUpdate = pacienteTratamientoCita.getIdSoporteNutricional() != null ?
                  soporteNutricionalRepository.updateNotificar(pacienteTratamientoCita.getIdSoporteNutricional()) :
                        Mono.empty();
                    return Mono.when(tratamientoUpdate, soporteNutricionalUpdate);
                })
                .then(Mono.just(Boolean.TRUE));
    }

    //historial remision
    @Override
    public Flux<RegistroHistorialRemision> consultarHistoricoRemision(String idRemision) {
        return registroHistorialRemisionRepository.findAllByIdRemision(idRemision)
                .map(ConverterAdmin::convertToRegistroHistoriaRemision);
    }

    @Override
    public Mono<RegistroHistorialRemision> consultarDatosRemision(String idRemision) {
        return registroHistorialRemisionRepository.buildByIdRemisionActual(idRemision)
                .map(ConverterAdmin::convertToRegistroHistoriaRemision);
    }

//profesionales
    @Override
    public Flux<Profesional> consultarProfesionales() {
        return profesionalRepository.findAll()
            .map(ConverterAdmin:: convertToProfesional);
        }

    @Override
    public Flux<Profesional> consultarProfesionalesByRegional(String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterAdmin::convertToProfesional)
                .sort(Comparator.comparing(Profesional::getNombres));
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
                .map(ConverterAdmin::convertToProfesional);
}

    @Override
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return Mono.just(profesional)
                .then(profesionalRepository.existsById(profesional.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(PROFESIONAL_NO_EXISTE.getValue()));
                    }
                    return profesionalRepository.save(ConverterAdmin.convertToProfesionalData(profesional));
                })
                .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                .map(ConverterAdmin::convertToProfesional);
    }
//moviles
    @Override
    public Mono<Movil> crearMovil(Movil movil) {
        return movilRepository.existsById(movil.getMatricula())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(MOVIL_YA_EXISTE.getValue()));
                    }
                    return movilRepository.insertMovil(movil);
                })
                .then(movilRepository.findById(movil.getMatricula()))
                .map(ConverterAdmin::convertToMovil);
    }

    @Override
    public Mono<Movil> actualizarMovil(Movil movil) {
        return Mono.just(movil)
                .then(movilRepository.existsById(movil.getMatricula()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(MOVIL_NO_EXISTE.getValue()));
                    }
                    return movilRepository.save(ConverterAdmin.convertToMovilData(movil));
                })
                .then(movilRepository.findById(movil.getMatricula()))
                .map(ConverterAdmin::convertToMovil);
    }

    @Override
    public Flux<Movil> consultarMoviles() {
        return movilRepository.findAll()
                .map(ConverterAdmin::convertToMovil);
    }

    //conductores
    @Override
    public Flux<Movil> consultarMovilesSinConductor() {
        return movilRepository.findAllWithoutConductor()
                .map(ConverterAdmin::convertToMovil);
    }

    @Override
    public Flux<Movil> consultarMovilesByIdRegional(String idRegional) {
        return movilRepository.findByIdRegional(idRegional)
                .map(ConverterAdmin::convertToMovil);
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
                .map(ConverterAdmin::converToConductor);
    }

    @Override
    public Mono<Conductor> actualizarConductor(Conductor conductor) {
        return Mono.just(conductor)
                .then(conductorRepository.existsById(conductor.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(CONDUCTOR_NO_EXISTE.getValue()));
                    }
                    return conductorRepository.save(ConverterAdmin.converToConductorData(conductor));
                })
                .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                .map(ConverterAdmin::converToConductor);
    }

    @Override
    public Flux<Conductor> consultarConductores() {
        return conductorRepository.findAll()
                .map(ConverterAdmin::converToConductor);
    }

    //turnos del personal
    @Override
    public Flux<ProfesionalWithTurno> consultarHorariosProfesionales(String fechaTurno, String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterAdmin::convertToProfesionalTurno)
                .flatMap(profesional -> turnoProfesionalesRepository
                        .findTurnoProfesionalByFechaRegional(
                                fechaTurno,profesional.getNumeroIdentificacion(), profesional.getIdRegional())
                        .map(ConverterAdmin::convertToTurnoProfesional)
                        .collectList()
                        .flatMap(turnos -> {
                            profesional.setTurnos(turnos);
                            return Mono.just(profesional);
                        }))
                .sort(Comparator.comparing(Profesional::getNombres));
    }

    @Override
    public Flux<Resultado> eliminarTurnosProfesionalesAccionMasiva(List<EliminarTurnoProfesionalRequest> turnoRequest) {
        return Flux.fromIterable(turnoRequest)
           .flatMap(turno ->citaRepository.findCitasByTurnoProfesional(turno.getFechaTurno(),turno.getIdProfesional())
               .hasElements()
               .flatMap(hasElment ->{
                  if(Boolean.FALSE.equals(hasElment)){
                    return turnoProfesionalesRepository
                             .eliminarByIdProfesionalFechaTurno(turno.getFechaTurno(),turno.getIdProfesional())
                             .then(Mono.just(Resultado.builder().build()));
                    }
                   return  Mono.just(GenericosFactory.crearResultado(
                            String.format(RESPUESTA_TURNO.getValue(),turno.getIdProfesional()), turno.getFechaTurno()));
                        })
                )
                .filter(Resultado::isNotNull);

    }


    @Override
    public Flux<Resultado> asignarTurnosProfesionalesAccionMasiva(List<TurnoProfesional> turnos) {
        return Flux.fromIterable(turnos)
                .flatMap(turno-> citaRepository.findCitasByTurnoProfesional(
                  turno.getFechaTurno(),turno.getIdProfesional())
                   .hasElements()
                   .flatMap(hasElment ->{
                    if(Boolean.FALSE.equals(hasElment)){
                       return turnoProfesionalesRepository.save(ConverterAdmin.convertToTurnoProfesionalData(turno))
                                 .then(Mono.just(Resultado.builder().build()));
                      }
                       return Mono.just(GenericosFactory.crearResultado(
                            String.format(RESPUESTA_TURNO.getValue(),turno.getIdProfesional()), turno.getFechaTurno()));
                   }))
                .filter(Resultado::isNotNull);

    }


    @Override
    public Mono<Boolean> actualizarHorarioTurnoProfesionales(List<TurnoProfesional> turnos) {

        return turnoProfesionalesRepository.eliminarByIdProfesionalFechaTurno(
                turnos.get(0).getFechaTurno(),turnos.get(0).getIdProfesional())
                .thenMany(Flux.fromIterable(turnos)
                        .map(ConverterAdmin::convertToTurnoProfesionalData)
                        .collectList()
                        .flatMapMany(turnoProfesionalesRepository::saveAll))
                .then(Mono.just(true));
    }
    @Override
    public Flux<Conductor> consultarHorariosConductores(LocalDate fechaTurno, String idRegional) {
        return null;
    }

    //secuencias turnos
    @Override
    public Flux<SecuenciaTurno> consultarSecuencias() {
        var horarioTurno  = horarioTurnoRepository.findAll()
                .map(ConverterMaestros::convertToHorarioTurno);
        return secuenciaTurnoRepository.findAll()
                .map(ConverterAdmin::convertToSecuenciaTurno)
                .flatMap(st -> {
                    var horariosTurnosFlux = horarioTurno
                            .filter(h -> h.getId().equals(st.getHorariosTurno().get(0).getId()))
                            .collectList();
                    return horariosTurnosFlux
                            .doOnNext(st::setHorariosTurno)
                            .thenReturn(st)
                            .map(SecuenciaTurno::crearSecuenciaTurnoFromItemsSecuencia);
                })
                .groupBy(SecuenciaTurno::getNombre)
                .flatMap(secuenciasTurnosAgrupados -> secuenciasTurnosAgrupados
                        .collectList()
                        .map(SecuenciaTurno::agruparItemsDiaTurno));

    }

    public Mono<Boolean> configurarSecuenciaTurno(SecuenciaTurno secuenciaTurno) {
        return secuenciaTurnoRepository.deleteByNombreSecuencia(secuenciaTurno.getNombre())
                        .thenMany(Flux.fromIterable(secuenciaTurno.getItemsDiaTurno())
                                .map(ItemDiaTurno::inicializarListaHorarioTurnoVacio)
                                .flatMap(itemDiaTurno -> Flux.fromIterable(itemDiaTurno.getHorariosTurno())
                                        .map(horarioTurno ->AdminDataFactory.crearItemDiaTurnoData(
                                                secuenciaTurno,itemDiaTurno,horarioTurno)))
                                .collectList()
                                .flatMapMany(secuenciaTurnoRepository::saveAll))
                .then(Mono.just(true));
    }
}
