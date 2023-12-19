package co.com.sura.postgres.remision.adapter;


import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.CuracionRequest;
import co.com.sura.remision.dto.DatosAtencionPacienteRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.dto.SecrecionRequest;
import co.com.sura.remision.dto.SondajeRequest;
import co.com.sura.remision.dto.SoporteNutricionalRequest;
import co.com.sura.remision.dto.TomaMuestraRequest;
import co.com.sura.remision.dto.TratamientoRequest;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Diagnostico;
import co.com.sura.remision.entity.datosremision.Medicamento;
import co.com.sura.remision.entity.datosremision.Paciente;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.remision.entity.datosremision.Ubicacion;
import co.com.sura.remision.entity.historial.CitaHistorial;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.remision.entity.procedimientos.Canalizacion;
import co.com.sura.remision.entity.procedimientos.Curacion;
import co.com.sura.remision.entity.procedimientos.Fototerapia;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import co.com.sura.remision.entity.procedimientos.Secrecion;
import co.com.sura.remision.entity.procedimientos.Sondaje;
import co.com.sura.remision.entity.procedimientos.SoporteNutricional;
import co.com.sura.remision.entity.procedimientos.TomaMuestra;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.remision.data.procedimientos.CanalizacionData;
import co.com.sura.postgres.remision.data.procedimientos.CuracionData;
import co.com.sura.postgres.remision.data.datospaciente.DatosAtencionPacienteData;
import co.com.sura.postgres.remision.data.procedimientos.FototerapiaData;
import co.com.sura.postgres.remision.data.datospaciente.PacienteData;
import co.com.sura.postgres.remision.data.datospaciente.RegistroHistorialRemisionData;
import co.com.sura.postgres.remision.data.datospaciente.RemisionData;
import co.com.sura.postgres.remision.data.datospaciente.RemisionDiagnosticoData;
import co.com.sura.postgres.remision.data.procedimientos.SecrecionData;
import co.com.sura.postgres.remision.data.procedimientos.SondajeData;
import co.com.sura.postgres.remision.data.procedimientos.SoporteNutricionalData;
import co.com.sura.postgres.remision.data.procedimientos.TomaMuestraData;
import co.com.sura.postgres.remision.data.tratamientos.TratamientoData;
import co.com.sura.postgres.remision.data.datospaciente.UbicacionData;
import io.r2dbc.postgresql.codec.Json;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Component
public class ConverterRemision extends Converter {

    protected static <T> T convertToJsonObject (Json jsonByteArrayInput,Class<T> clazz ){
        if(jsonByteArrayInput!=null){
            byte[] byteArray = jsonByteArrayInput.asArray();
            var jsonString = new String(byteArray);
            return  Converter.deserializarJson(jsonString, clazz);
        }else{
            return null;
        }

    }
    protected static Json convertToJsonb(Object object){
        return Json.of(Objects.requireNonNull(Converter.convertirObjetoAJson(object)));


    }
    public static RemisionData convertToRemisionRequest(RemisionRequest remisionRequest){
        return new RemisionData()
                .toBuilder()
                .idRemision(remisionRequest.getIdRemision())
                .institucionRemite(remisionRequest.getInstitucionRemite())
                .programa(remisionRequest.getPrograma().getNombre())
                .fechaAdmision(remisionRequest.getFechaAdmision())
                .tipoAdmision(remisionRequest.getTipoAdmision())
                .numeroIdentificacionPaciente(remisionRequest.getNumeroIdentificacion())
                .estado(remisionRequest.getEstado())
                .idRegional(remisionRequest.getRegional().getIdRegional())
                .build();
    }
    public static RegistroHistorialRemision buildHistorialRemision(RemisionData remisionData){
        return RegistroHistorialRemision.builder()
                .idRemision(remisionData.getIdRemision())
                .estado(remisionData.getEstado())
                .fechaAdmision(remisionData.getFechaAdmision())
                .programa(remisionData.getPrograma())
                .tipoAdmision(remisionData.getTipoAdmision())
                .institucionRemite(remisionData.getInstitucionRemite())
                .paciente(Paciente.builder()
                        .numeroIdentificacion(remisionData.getNumeroIdentificacionPaciente())
                        .build())
                .build();
    }
    public static  Paciente convertToPaciente (PacienteData pacienteData){
        return new Paciente()
                .toBuilder()
                .tipoIdentificacion(pacienteData.getTipoIdentificacion())
                .numeroIdentificacion(pacienteData.getNumeroIdentificacion() )
                .nombres(pacienteData.getNombres())
                .apellidos(pacienteData.getApellidos())
                .sexo(pacienteData.getSexo())
                .peso(pacienteData.getPeso())
                .edad(pacienteData.getEdad())
                .tipoAfiliacion(pacienteData.getTipoAfiliacion())
                .nombreAseguradora(pacienteData.getNombreAseguradora())
                .fechaNacimiento(pacienteData.getFechaNacimiento())
                .build();
    }
    public static Ubicacion convertToUbicacion(UbicacionData ubicacionData){
        return converToEntity(ubicacionData, Ubicacion.class);
    }

    public static UbicacionData extraerUbicacionData(RemisionRequest remisionRequest){
        var ubicacionRequest = remisionRequest.getDatosAtencionPaciente().getUbicacion();
        return new UbicacionData()
                .toBuilder()
                .idUbicacion(remisionRequest.getNumeroIdentificacion()+"_ubicacion")
                .latitud(ubicacionRequest.getLatitud())
                .longitud(ubicacionRequest.getLongitud())
                .direccion(ubicacionRequest.getDireccion())
                .tipoVia(ubicacionRequest.getTipoVia())
                .numero1(ubicacionRequest.getNumero1())
                .numeroInterseccion(ubicacionRequest.getNroInterseccion())
                .numero2(ubicacionRequest.getNumero2())
                .barrio(ubicacionRequest.getBarrio())
                .sinNomenclatura(ubicacionRequest.isSinNomenclatura())
                .municipio(ubicacionRequest.getMunicipio().getNombre())
                .idRegional(remisionRequest.getRegional().getIdRegional())
                .build();
    }

    public static PacienteData extraerPacienteData(RemisionRequest remisionRequest){
        return new PacienteData()
                .toBuilder()
                .tipoIdentificacion(remisionRequest.getTipoIdentificacion().getNombre())
                .numeroIdentificacion(remisionRequest.getNumeroIdentificacion())
                .nombres(remisionRequest.getNombre())
                .apellidos(remisionRequest.getApellido())
                .edad(remisionRequest.getEdad())
                .sexo(remisionRequest.getSexo())
                .peso(remisionRequest.getPeso())
                .tipoAfiliacion(remisionRequest.getTipoAfiliacion().getNombre())
                .nombreAseguradora(remisionRequest.getTipoAfiliacion().getNombreAseguradora())
                .fechaNacimiento(remisionRequest.getFechaNacimiento())
                .idUbicacion(remisionRequest.getNumeroIdentificacion()+"_ubicacion")
                .build();
    }
    public static List<RemisionDiagnosticoData> extraerRemisionDiagnosticoData(
            Collection<Diagnostico> diagnosticos, String idRemision){

        return diagnosticos
                .stream()
                .map(diagnostico -> new RemisionDiagnosticoData()
                        .toBuilder()
                        .idRemision(idRemision)
                        .nombreDiagnostico(diagnostico.getNombre())
                        .codigo(diagnostico.getCodigo())
                        .build())
                .collect(Collectors.toList());
    }

    public static CitaData convertirCitaData(CitaRequest citaRequest){
        return new CitaData()
                .toBuilder()
                .idCita(citaRequest.getIdCita())
                .duracion(citaRequest.getDuracion())
                .holgura(citaRequest.getHolgura())
                .fechaInicio(citaRequest.getFechaInicio())
                .especialidad(citaRequest.getEspecialidad())
                .idCita(citaRequest.getIdCita())
                .idEstado(EstadosCita.SIN_AGENDAR.getEstado())
                .fechaProgramada(citaRequest.getFechaInicio())
                .build();
    }

    public static List<CitaData> convertirCitasDataList(
            Collection<CitaRequest> citasRequest, RemisionRequest remisionRequest){

        return citasRequest
                .stream()
                .map(ConverterRemision:: convertirCitaData)
                .peek(citaData -> citaData.setIdRemision(remisionRequest.getIdRemision()))
                .peek(citaData -> citaData.setIdRegional(remisionRequest.getRegional().getIdRegional()))
                .peek(citaData -> citaData.setLatitud(remisionRequest
                        .getDatosAtencionPaciente().getUbicacion().getLatitud()))
                .peek(citaData -> citaData.setLongitud(remisionRequest
                        .getDatosAtencionPaciente().getUbicacion().getLongitud()))
                .collect(Collectors.toList());
    }
    public static List<Tratamiento> builTratamientosFromRequest(CitaRequest citaRequest){
        return citaRequest.getTratamientos().stream()
                .map(ConverterRemision::extraerTratamientoData)
                .map(ConverterRemision::converterToTratamiento)
                .collect(Collectors.toList());
    }
    public static Procedimientos builProcedimientosFromRequest(CitaRequest citaRequest){
        return Procedimientos.builder()
                .curaciones(citaRequest.getProcedimientos().getCuraciones()
                        .stream()
                        .map(ConverterRemision::extraerCuracionData)
                        .map(curacionData -> converToEntity(curacionData, Curacion.class))
                        .collect(Collectors.toList()))
                .canalizaciones(citaRequest.getProcedimientos().getCanalizaciones())
                .fototerapias(citaRequest.getProcedimientos().getFototerapias())
                .secreciones(citaRequest.getProcedimientos().getSecreciones()
                        .stream()
                        .map(ConverterRemision::extraerSecrecionData)
                        .map(secrecionData -> converToEntity(secrecionData, Secrecion.class))
                        .collect(Collectors.toList()))
                .sondajes(citaRequest.getProcedimientos().getSondajes()
                        .stream()
                        .map(ConverterRemision::convertirSondajeData)
                        .map(sondajeData->converToEntity(sondajeData, Sondaje.class))
                        .collect(Collectors.toList()))
                .soporteNutricionales(citaRequest.getProcedimientos().getSoporteNutricionales()
                        .stream()
                        .map(ConverterRemision::extraerSoporteNutricionalData)
                        .map(ConverterRemision::convertToSoporteNutricional)
                        .collect(Collectors.toList()))
                .tomaMuestras(citaRequest.getProcedimientos().getTomaMuestras()
                        .stream()
                        .map(ConverterRemision::extraerTomaMuestra)
                        .map(tomaMuestraData->converToEntity(tomaMuestraData, TomaMuestra.class))
                        .collect(Collectors.toList()))
                .build();
    }
    public static List<CitaHistorial> buildCitaHistorialFromRequest(Collection<CitaRequest> citasRequest,
                                                                    RemisionRequest remisionRequest, Integer idUltimo){
        var idCita = new AtomicInteger(idUltimo+1);
         return citasRequest.stream()
                         .map(citaRequest->   CitaHistorial.builder()
                                 .idCita(remisionRequest.getIdRemision()+"-"+ idCita.getAndIncrement())
                                 .idRemision(remisionRequest.getIdRemision())
                                 .idRegional(remisionRequest.getRegional().getIdRegional())
                                 .idEstado(EstadosCita.SIN_AGENDAR.getEstado())
                                 .duracion(citaRequest.getDuracion())
                                 .holgura(citaRequest.getHolgura())
                                 .fechaInicio(citaRequest.getFechaInicio())
                                 .fechaProgramada(citaRequest.getFechaInicio())
                                 .latitud(remisionRequest.getDatosAtencionPaciente().getUbicacion().getLatitud())
                                 .longitud(remisionRequest.getDatosAtencionPaciente().getUbicacion().getLongitud())
                                 .especialidad(citaRequest.getEspecialidad())
                                 .tratamientos(builTratamientosFromRequest(citaRequest))
                                 .procedimientos(builProcedimientosFromRequest(citaRequest)).build())
                 .collect(Collectors.toList());

    }

    public static DatosAtencionPacienteData extraerDatosAtencionPacienteData(
            DatosAtencionPacienteRequest datosAtencionPacienteRequest, String idRemision){

        return new DatosAtencionPacienteData()
                .toBuilder()
                .nombreCuidador(datosAtencionPacienteRequest.getNombreCuidador())
                .nombreResponsable(datosAtencionPacienteRequest.getNombreResponsable())
                .telefonoPaciente(datosAtencionPacienteRequest.getTelefonoPaciente())
                .celularPaciente(datosAtencionPacienteRequest.getCelularPaciente())
                .celularPaciente2(datosAtencionPacienteRequest.getCelularPaciente2())
                .idRemision(idRemision)
                .build();
    }
    protected static TratamientoData extraerTratamientoData(TratamientoRequest tratamientoRequest){

        return new TratamientoData()
                .toBuilder()
                .tipoTratamiento(tratamientoRequest.getTipoTratamiento().getNombre())
                .idMedicamento(tratamientoRequest.getMedicamento().getIdMedicamento())
                .nombreMedicamento(tratamientoRequest.getMedicamento().getNombre())
                .codigoMedicamento(tratamientoRequest.getMedicamento().getCodigoMedicamento())
                .presentacionMedicamento(tratamientoRequest.getMedicamento().getPresentacion())
                .cantidadDosis((int) tratamientoRequest.getCantidadDosis())
                .unidadDosis(tratamientoRequest.getUnidadDosis().getDescripcion())
                .viaAdministracion(tratamientoRequest.getViaAdministracion().getDescripcion())
                .frecuencia(tratamientoRequest.getFrecuencia().getDescripcion())
                .duracion((int) tratamientoRequest.getDuracion())
                .noPBS(tratamientoRequest.getNoPBS())
                .tipoPrestacion(tratamientoRequest.getTipoPrestacion().getTipoPrestacion())
                .build();
    }
    public  static  List<TratamientoData> extraerTratamientoData(List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getTratamientos()
                        .stream()
                        .map(ConverterRemision::extraerTratamientoData)
                        .peek(tratamientoData -> tratamientoData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    public static Tratamiento converterToTratamiento(TratamientoData tratamientoData){
        return Tratamiento.builder()
                .tipoTratamiento(tratamientoData.getTipoTratamiento())
                .medicamento(Medicamento.builder()
                        .idMedicamento(tratamientoData.getIdMedicamento())
                        .codigoMedicamento(tratamientoData.getCodigoMedicamento())
                        .nombre(tratamientoData.getNombreMedicamento())
                        .presentacion(tratamientoData.getPresentacionMedicamento())
                        .build())
                .cantidadDosis(tratamientoData.getCantidadDosis())
                .unidadDosis(tratamientoData.getUnidadDosis())
                .viaAdministracion(tratamientoData.getViaAdministracion())
                .frecuencia(tratamientoData.getFrecuencia())
                .duracion(tratamientoData.getDuracion())
                .noPBS(tratamientoData.getNoPBS())
                .tipoPrestacion(tratamientoData.getTipoPrestacion())
                .notificado(tratamientoData.isNotificado())
                .build();
    }

    protected static CanalizacionData convertirCanalizacionData(Canalizacion canalizacion ){
        return converToEntity(canalizacion, CanalizacionData.class);
    }

    public  static  List<CanalizacionData> extraerCanalizacionData (Collection<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getCanalizaciones()
                        .stream()
                        .map(ConverterRemision:: convertirCanalizacionData)
                        .peek(canalizacionData -> canalizacionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static FototerapiaData convertirFototerapiaData(Fototerapia fototerapia ){
        return converToEntity(fototerapia, FototerapiaData.class);
    }

    public  static  List<FototerapiaData> extraerFototerapiaData (Collection<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getFototerapias()
                        .stream()
                        .map(ConverterRemision:: convertirFototerapiaData)
                        .peek(fototerapiaData -> fototerapiaData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static SecrecionData extraerSecrecionData(SecrecionRequest secrecionRequest ){
        return converToEntity(secrecionRequest, SecrecionData.class);
    }

    public  static  List<SecrecionData> extraerSecrecionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSecreciones()
                        .stream()
                        .map(ConverterRemision::extraerSecrecionData)
                        .peek(secrecionData -> secrecionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static SondajeData convertirSondajeData(SondajeRequest sondajeRequest ){
        return new SondajeData()
                .toBuilder()
                .tipoSondaje(sondajeRequest.getSondaje())
                .tipoSonda(sondajeRequest.getTipoSondaje())
                .totalSesiones(sondajeRequest.getTotalSesiones())
                .tipoPrestacion(sondajeRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SondajeData> extraerSondajeData (Collection<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSondajes()
                        .stream()
                        .map(ConverterRemision:: convertirSondajeData)
                        .peek(sondajeData -> sondajeData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    protected static SoporteNutricionalData extraerSoporteNutricionalData(
            SoporteNutricionalRequest soporteNutricionalRequest ){

        return new SoporteNutricionalData()
                .toBuilder()
                .descripcion(soporteNutricionalRequest.getDescripcion())
                .tipo(soporteNutricionalRequest.getTipo())
                .unidadDosis(soporteNutricionalRequest.getUnidadDosis().getDescripcion())
                .duracion(soporteNutricionalRequest.getDuracion())
                .volumen(soporteNutricionalRequest.getVolumen())
                .idMedicamento(soporteNutricionalRequest.getMedicamento().getIdMedicamento())
                .nombreMedicamento(soporteNutricionalRequest.getMedicamento().getNombre())
                .codigoMedicamento(soporteNutricionalRequest.getMedicamento().getCodigoMedicamento())
                .presentacionMedicamento(soporteNutricionalRequest.getMedicamento().getPresentacion())
                .cantidadDosis(soporteNutricionalRequest.getCantidadDosis())
                .noPBS(soporteNutricionalRequest.isNoPBS())
                .tipoPrestacion(soporteNutricionalRequest.getTipoPrestacion())
                .build();
    }
    protected static SoporteNutricional convertToSoporteNutricional(
            SoporteNutricionalData soporteNutricionalData){

        return new SoporteNutricional()
                .toBuilder()
                .medicamento(Medicamento.builder()
                        .idMedicamento(soporteNutricionalData.getIdMedicamento())
                        .nombre(soporteNutricionalData.getNombreMedicamento())
                        .presentacion(soporteNutricionalData.getPresentacionMedicamento())
                        .codigoMedicamento(soporteNutricionalData.getCodigoMedicamento())
                        .build())
                .descripcion(soporteNutricionalData.getDescripcion())
                .tipo(soporteNutricionalData.getTipo())
                .unidadDosis(soporteNutricionalData.getUnidadDosis())
                .duracion(soporteNutricionalData.getDuracion())
                .volumen(soporteNutricionalData.getVolumen())
                .cantidadDosis(soporteNutricionalData.getCantidadDosis())
                .noPBS(soporteNutricionalData.getNoPBS())
                .tipoPrestacion(soporteNutricionalData.getTipoPrestacion())
                .build();
    }
    public  static  List<SoporteNutricionalData> extraerSoporteNutricionalData(
            Collection<CitaRequest> listacitasRequest) {

        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSoporteNutricionales()
                        .stream()
                        .map(ConverterRemision::extraerSoporteNutricionalData)
                        .peek(soporteNutricionalData -> soporteNutricionalData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    protected static TomaMuestraData extraerTomaMuestra(TomaMuestraRequest tomaMuestraRequest ){
       return new TomaMuestraData()
                .toBuilder()
                .tipoMuestra(tomaMuestraRequest.getTipoMuestra().getDescripcion())
                .requiereAyuno(tomaMuestraRequest.isRequiereAyuno())
                .tipoPrestacion(tomaMuestraRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<TomaMuestraData> extraerSoporteTomaMuestraData (Collection<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getTomaMuestras()
                        .stream()
                        .map(ConverterRemision:: extraerTomaMuestra)
                        .peek(tomaMuestraData ->  tomaMuestraData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static CuracionData extraerCuracionData(CuracionRequest curacionRequest ){

        return new CuracionData()
                .toBuilder()
                .tipoCuracion(curacionRequest.getTipoCuracion().getDescripcion())
                .descripcion(curacionRequest.getDescripcion())
                .sesiones(curacionRequest.getSesiones())
                .build();
    }

    public  static  List<CuracionData> extraerCuracionData (Collection<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getCuraciones()
                        .stream()
                        .map(ConverterRemision::extraerCuracionData)
                        .peek(curacionData ->  curacionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    public static DatosAtencionPaciente convertToDatosAtencionPaciente(
            DatosAtencionPacienteData datosAtencionPacienteData){
        return converToEntity(datosAtencionPacienteData, DatosAtencionPaciente.class);
    }

    public static  RegistroHistorialRemision convertToRegistroHistoriaRemision(
            RegistroHistorialRemisionData registroHistorialRemisionData){

        var builder = new RegistroHistorialRemision().toBuilder()
              .idRemision(registroHistorialRemisionData.getIdRemision())
              .fechaAplicacionNovedad(registroHistorialRemisionData.getFechaAplicacionNovedad())
              .motivoNovedad(registroHistorialRemisionData.getMotivoNovedad())
              .estado(registroHistorialRemisionData.getEstado())
              .fechaAdmision(registroHistorialRemisionData.getFechaAdmision())
              .programa(registroHistorialRemisionData.getPrograma())
              .tipoAdmision(registroHistorialRemisionData.getTipoAdmision())
              .institucionRemite(registroHistorialRemisionData.getInstitucionRemite())
              .paciente(convertToJsonObject(registroHistorialRemisionData.getPaciente(),Paciente.class))
              .datosAtencion(convertToJsonObject(registroHistorialRemisionData.getDatosAtencion(),Object.class))
              .ubicacionPaciente(convertToJsonObject(registroHistorialRemisionData.getUbicacionPaciente(),Object.class))
              .diagnosticos(convertToJsonObject(registroHistorialRemisionData.getDiagnosticos(),Object.class));

        if (registroHistorialRemisionData.getCitas() != null) {
            builder.citas(convertToJsonObject(registroHistorialRemisionData.getCitas(), LinkedList.class));
        }
        if (registroHistorialRemisionData.getCitasNuevas() != null) {
            builder.citasNuevas(convertToJsonObject(registroHistorialRemisionData.getCitasNuevas(), LinkedList.class));
        }
        return builder.build();
    }
    public static  RegistroHistorialRemisionData convertToRegistroHistoriaRemisionData(
            RegistroHistorialRemision registroHistorialRemision){

        var builder = new RegistroHistorialRemisionData().toBuilder()
                .idRemision(registroHistorialRemision.getIdRemision())
                .fechaAplicacionNovedad(registroHistorialRemision.getFechaAplicacionNovedad())
                .motivoNovedad(registroHistorialRemision.getMotivoNovedad())
                .estado(registroHistorialRemision.getEstado())
                .fechaAdmision(registroHistorialRemision.getFechaAdmision())
                .programa(registroHistorialRemision.getPrograma())
                .tipoAdmision(registroHistorialRemision.getTipoAdmision())
                .institucionRemite(registroHistorialRemision.getInstitucionRemite())
                .paciente(convertToJsonb(registroHistorialRemision.getPaciente()))
                .datosAtencion(convertToJsonb(registroHistorialRemision.getDatosAtencion()))
                .ubicacionPaciente(convertToJsonb(registroHistorialRemision.getUbicacionPaciente()))
                .diagnosticos(convertToJsonb(registroHistorialRemision.getDiagnosticos()));

        if (registroHistorialRemision.getCitas() != null) {
            builder.citas(convertToJsonb(registroHistorialRemision.getCitas()));
        }
        if (registroHistorialRemision.getCitasNuevas() != null) {
            builder.citasNuevas(convertToJsonb(registroHistorialRemision.getCitasNuevas()));
        }
        return builder.build();
    }
}
